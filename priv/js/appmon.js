var $ = $ || {};
var $jit = $jit || {};

APPMON = (function() {
    var started = false;
    function _start(nodeName) {
        if(started) return;
        started = true;
        var getFun = function(mfa) {
            return 'fun ' + mfa[0] + ':' + mfa[1] + '/' + mfa[2];
        };

        var fetchJson = function(nodeName, callback) {
            var json = {
                id: 'rootNode',
                name: nodeName,
                data: {
                    name: nodeName,
                    nodeType: 'node'
                },
                children: []
            };

            $.getJSON('/appmon/_all', function(data) {
                var nodes = {};
                var getMap = $.map(data, function(info, pid) {
                    var child = {
                        id: 'master-' + pid,
                        name: 'master-' + pid,
                        data: $.extend(info, {
                            nodeType: 'appMaster',
                            pid: pid
                        }),
                        children: []
                    };
                    json.children.push(child);
                    nodes[pid] = child;
                    return $.getJSON('/appmon/' + info.name, function(appData) {
                        $.extend(nodes[pid].data, appData.p[pid]);
                        var links = {};
                        $.each(appData.l, function(index, linkInfo) {
                            var parentPid = linkInfo[0];
                            var childPid = linkInfo[1];
                            if (!(parentPid in links)) {
                                links[parentPid] = [];
                            }
                            links[parentPid].push(childPid);
                        });
                        $.each(appData.p, function(childPid, pidInfo) {
                            if (!(childPid in nodes)) {
                                nodes[childPid] = {
                                    id: 'pid-' + childPid,
                                    name: 'pid-' + childPid,
                                    data: $.extend(pidInfo, {
                                        nodeType: 'normal',
                                        pid: childPid
                                    }),
                                    children: []
                                };
                            }
                        });
                        $.each(links, function(parent, children) {
                            if (parent in nodes) {
                                $.each(children, function(index, child) {
                                    if (child in nodes) {
                                        nodes[parent].children.push(nodes[child]);
                                    } else if (child._type == 'port') {
                                        nodes[parent].children.push({
                                            id: 'port-' + child.data,
                                            name: 'port-' + child.data,
                                            data: {
                                                nodeType: 'port',
                                                port: child.data
                                            },
                                            children: []
                                        });
                                    } else {
                                        console.log('could not find child ' + JSON.stringify(child));
                                    }
                                });
                            }
                        });
                    });
                });

                $.when.apply($, getMap).then(function() {
                    callback(json);
                });
            });
        };

        var st = new $jit.ST({
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
                        label.append('<span class="app-name">' + node.data.name + '</span>');
                        label.append('<span class="app-version">' + node.data.ver + '</span>');
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
            onBeforePlotNode: function(node){
                if (node.selected) {
                    // node.data.$color = "#000";
                } else {
                    // node.data.$color = "#fff";
                }
            },
            onBeforePlotLine: function(adj){
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

        var refreshTree = function() {
            fetchJson(nodeName, function(json) {
                st.loadJSON(json);
                st.refresh();
                if (st.selectedNode) {
                    st.select(st.selectedNode);
                } else {
                    st.onClick(st.root);
                }
            });
        };

        $('#refresh').bind('click', function(event) {
            event.preventDefault();
            refreshTree();
        });

        fetchJson(nodeName, function(json) {
            st.loadJSON(json);
            st.compute();
            st.onClick(st.root);
        });
    }
    return {
        start: _start
    };
})();
