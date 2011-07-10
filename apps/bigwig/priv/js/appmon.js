var $ = $ || {};
var $jit = $jit || {};
$(function() {
    var getFun = function(mfa) {
        return 'fun ' + mfa[0] + ':' + mfa[1] + '/' + mfa[2];
    };

    $.getJSON('/appmon/_all', function(data) {
        var nodeName = 'bigwig@virding';

        var json = {
            id: 'nodeName',
            name: 'nodeName',
            data: {
                name: nodeName,
                nodeType: 'node'
            },
            children: []
        };

        var nodes = {};

        var i = 0;

        var getMap = $.map(data, function(info, pid) {
            i += 1;
            var child = {
                id: 'node' + i,
                name: 'node' + i,
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
                        i += 1;
                        nodes[childPid] = {
                            id: 'node' + i,
                            name: 'node' + i,
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
                                i += 1;
                                nodes[parent].children.push({
                                    id: 'node' + i,
                                    name: 'node' + i,
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
            var st = new $jit.ST({
                injectInto: 'appmon-graph',
                duration: 400,
                transition: $jit.Trans.Quart.easeInOut,
                levelDistance: 50,
                levelsToShow: 3,
                constrained: false,
                Navigation: {
                    enable: true,
                    panning: true
                },
                Node: {
                    height: 120,
                    width: 300,
                    type: 'ellipse',
                    color: '#0aa',
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
                onCreateLabel: function(label, node) {
                    label.id = node.id;

                    label.innerHTML = "";

                    // Set up label
                    if ('pid' in node.data) {
                        label.innerHTML += '<span class="pid">'
                            + node.data.pid + '</span>';
                    } else if ('port' in node.data) {
                        label.innerHTML += '<span class="pid">'
                            + node.data.port + '</span>';
                    }
                    if (node.data.nodeType == 'appMaster') {
                        label.innerHTML += '<span class="app-name">'
                            + node.data.name
                            + '</span><span class="app-version">'
                            + node.data.ver + '</span>';
                    } else if (node.data.nodeType == 'node') {
                        label.innerHTML += node.data.name;
                    } else if ('name' in node.data) {
                        label.innerHTML += '<span class="registered-name">' + node.data.name + '</span>';
                    }
                    if ('cf' in node.data) {
                        label.innerHTML += '<span class="pid-cf">' + getFun(node.data.cf) + '</span>';
                    }
                    if ('ic' in node.data) {
                        label.innerHTML += '<span class="pid-ic">' + getFun(node.data.ic) + '</span>';
                    }
                    if ('q' in node.data) {
                        label.innerHTML += '<span class="mq-len">Msgs: ' + node.data.q + '</span>';
                    }

                    label.onclick = function() {
                        st.onClick(node.id, {
                            Move: {
                                offsetY: -90
                            }
                        });
                    };
                },
                onBeforePlotNode: function(node){
                    if (node.selected) {
                        node.data.$color = "#ff7";
                    }
                    else {
                        delete node.data.$color;
                        // If the node belongs to the last plotted level
                        if(!node.anySubnode("exist")) {
                            var count = 0;
                            node.eachSubnode(function(n) { count++; });
                            // Assign a node color based on how many children it has
                            node.data.$color = ['#aaa', '#baa', '#caa', '#daa', '#eaa', '#faa'][count];
                        }
                    }
                },
                onBeforePlotLine: function(adj){
                    if (adj.nodeFrom.selected && adj.nodeTo.selected) {
                        adj.data.$color = "#46e";
                        adj.data.$lineWidth = 4;
                    }
                    else {
                        delete adj.data.$color;
                        delete adj.data.$lineWidth;
                    }
                }
            });
            st.loadJSON(json);
            st.compute();
            st.onClick(st.root, {
                Move: {
                    offsetY: -90
                }
            });
        });
    });
});
