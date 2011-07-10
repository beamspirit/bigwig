var $ = $ || {};
var $jit = $jit || {};

APPMON = (function() {
    var started = false;
    var liveEnabled = true;
    var lastTree = {};
    var nodeName = 'nonode@nohost';
    var st = undefined;
    var initialised = false;
    var nodes = {};
    var nodesById = {};
    var apps = {};

    function updateApps(data) {
        // TODO: this is hideous
        apps = {};
        $.each(data, function(index, app) {
            var pid = app.data[0].data;
            apps[pid] = {
                name: app.data[2].data[0],
                desc: app.data[2].data[1],
                ver: app.data[2].data[2]
            };
        });
    }

    function getJSONTree() {
        var json = {
            id: 'rootNode',
            name: nodeName,
            data: {
                name: nodeName,
                nodeType: 'node'
            },
            children: []
        };
        nodesById = {'rootNode': json};
        nodes = {};

        $.each(lastTree, function(appName, appData) {
            var pid = appData.root;
            var nodeId = 'master-' + pid;
            var info = appData.p[pid];
            if (pid in apps) {
                $.extend(info, apps[pid]);
            }
            var child = {
                id: nodeId,
                name: nodeId,
                data: $.extend(info, {
                    nodeType: 'appMaster',
                    pid: pid
                }),
                children: []
            };
            json.children.push(child);
            nodes[pid] = child;
            nodesById[nodeId] = child;

            // Create links lookup table
            var links = {};
            $.each(appData.l, function(index, linkInfo) {
                var parentPid = linkInfo[0];
                var childPid = linkInfo[1];
                if (!(parentPid in links)) {
                    links[parentPid] = [];
                }
                links[parentPid].push(childPid);
            });

            // Iterate over pids and set up nodes
            $.each(appData.p, function(childPid, pidInfo) {
                if (!(childPid in nodes)) {
                    var nodeId = 'pid-' + childPid;
                    nodes[childPid] = {
                        id: nodeId,
                        name: nodeId,
                        data: $.extend(pidInfo, {
                            nodeType: 'normal',
                            pid: childPid
                        }),
                        children: []
                    };
                    nodesById[nodeId] = nodes[childPid];
                }
            });

            // Wire up links
            $.each(links, function(parent, children) {
                if (parent in nodes) {
                    $.each(children, function(index, child) {
                        if (child in nodes) {
                            nodes[parent].children.push(nodes[child]);
                        } else if (child._type == 'port') {
                            var portNodeId = 'port-' + child.data;
                            var portNode = {
                                id: portNodeId,
                                name: portNodeId,
                                data: {
                                    nodeType: 'port',
                                    port: child.data
                                },
                                children: []
                            };
                            nodes[parent].children.push(portNode);
                            nodesById[portNodeId] = portNode;
                        } else {
                            console.log('could not find child ' + JSON.stringify(child));
                        }
                    });
                }
            });
        });

        return json;
    }

    function _start(newNodeName) {
        if (started) return;
        started = true;
        nodeName = newNodeName;
        st = new $jit.ST({
            injectInto: 'appmon-graph',
            duration: 400,
            transition: $jit.Trans.Quart.easeInOut,
            levelDistance: 50,
            levelsToShow: 3,
            constrained: true,
            Node: {
                height: 60,
                width: 97,
                type: 'rectangle',
                color: '#ccc',
                overridable: true
            },
            Edge: {
                type: 'quadratic:end',
                overridable: true
            },
            onBeforeCompute: function(node) {
                // Loading
            },
            onAfterCompute: function() {
                // Done loading
            },
            onCreateLabel: function(labelObj, node) {
                var label = $(labelObj);
                label.attr('id', node.id);

                label.bind('click', function() {
                    st.selectedNode = node.id;
                    st.onClick(node.id);
                });

                if ('pid' in node.data) {
                    label.append(RENDERER.render_json_val({'_type': 'pid', 'data': node.data.pid}));
                    if (node.data.nodeType == 'appMaster') {
                        if ('name' in node.data && 'ver' in node.data) {
                            label.append('<span class="app-name">' + node.data.name + '</span>');
                            label.append('<span class="app-version">' + node.data.ver + '</span>');
                        }
                    } else if ('name' in node.data) {
                        label.append('<span class="registered-name">' + node.data.name + '</span>');
                    }
                } else if ('port' in node.data) {
                    label.append('<span class="pid">' + node.data.port + '</span>');
                } else if (node.data.nodeType == 'node') {
                    label.append('<span class="node-name">' + node.data.name + '</span>');
                }

                if ('q' in node.data && node.data.q > 0) {
                    label.append('<span class="messages">' + node.data.q + '<span class="inbox">&#x2709;</span></span>');
                }
            },
            onBeforePlotNode: function(node) {
            },
            onBeforePlotLine: function(adj) {
                if (adj.nodeFrom.selected && adj.nodeTo.selected) {
                    adj.data.$color = "#46e";
                    adj.data.$lineWidth = 2;
                }
                else {
                    delete adj.data.$color;
                    delete adj.data.$lineWidth;
                }
            }
        });
    }

    function refresh() {
        if (!st) {
            return;
        }
        if ('selectedNode' in st
            && st.selectedNode
            && !(st.selectedNode in nodesById)) {
            console.log('selected node (' + st.selectedNode + ') disappeared, returning to root node...');
            st.select(st.root);
            delete st.selectedNode;
        }
        if (!st.busy) {
            st.loadJSON(getJSONTree());
            if (initialised) {
                st.refresh();
                if (st.selectedNode) {
                    st.select(st.selectedNode);
                } else {
                    st.onClick(st.root);
                }
            } else {
                initialised = true;
                st.compute();
                st.onClick(st.root);
            }
        }
    }

    function updateTree(data) {
        lastTree = data;
        if (liveEnabled) {
            refresh();
        }
    }

    return {
        start: _start,
        updateTree: updateTree,
        updateApps: updateApps,
        refresh: refresh,
        enabled: function() {
            return liveEnabled;
        },
        enable: function() {
            liveEnabled = true;
        },
        disable: function() {
            liveEnabled = false;
        },
        getJSONTree: getJSONTree
    };
})();

$(function() {
    $('#appmon-live-toggle').bind('click', function(event) {
        event.preventDefault();
        if (APPMON.enabled()) {
            APPMON.disable();
            $(this).text('Enable live updates');
        } else {
            APPMON.enable();
            $(this).text('Disable live updates');
            APPMON.refresh();
        }
    });

    $('#appmon').bind('onupdate', function(e, data) {
        if (data._type == 'tuple' && data.data[0] == 'node_app_tree') {
            // Handle tree update
            APPMON.updateTree(data.data[1]);
        } else if (data._type == 'tuple' && data.data[0] == 'node_apps') {
            APPMON.updateApps(data.data[1]);
        }
    });
});
