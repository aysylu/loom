(ns loom.protocols)

(defprotocol PGraph
  "Minimal functionality of a graph."
  (directed? [g]
    "Returns true if the graph is directed and false if the
     graph is undirected. If it is undirected, all functions
     taking two nodes must be commutative with regard to
     these nodes.")
  (nodes [g]
    "Returns a set or sequence of all nodes of the graph. May
     not contain duplicates.")
  (has-node? [g n]
    "Returns true if the graph g contains the node n.")
  (has-edge? [g n1 n2]
    "Returns true if the graph g has an edge from node n1
     to node n2.")
  (direct-successors [g n]
    "Returns a set or sequence of all nodes n2 for which
     (has-edge? g n n2) returns true. May not contain
     duplicates."))

(defprotocol PPredecessorGraph
  "Optional functionality of a graph which can give a
   list of all direct predecessors of a node."
  (direct-predecessors [g n]
    "Returns a set or sequence of all nodes n2 for which
     (has-edge? g n2 n) returns true. May not contain
     duplicates."))

(defprotocol PEditableGraph
  "Minimal functionality of an editable graph."
  (mutable? [g]
    "Returns true if the graph is mutated in place.
     If true is returned, the other functions change
     the graph passed as the first argument and return
     the same graph object. If false is returned, the
     functions return a new graph and the old graph is
     unchaged.")
  (add-node [g n]
    "Adds the node n to the graph g. If it already
     contained n, the graph will not be changed.")
  (remove-node [g n]
    "Removes the node n from the graph g. If it did
     not contain n, the graph will not be changed.")
  (add-edge [g n1 n2]
    "Adds an edge from node n1 to node n2 to the graph g.
     If one or both of the nodes is not present it will
     be added to the graph. If the edge was already present,
     the graph will not be changed.")
  (remove-edge [g n1 n2]
    "Removes the edge from node n1 to the node n2 from
     the graph g. If it did not contain the edge, the graph
     will not be changed."))

(defprotocol PWeightedGraph
  "Functionality of a graph whose edges can be weighted."
  (edge-weight [g n1 n2]
    "Returns the weight of the edge from node n1 to
     node n2."))

(defprotocol PEditableWeightedGraph
  "Functionality of a weighted graph whose weights can be
   changed."
  (update-edge-weight [g n1 n2 f]
    "Updates the weight of the edge from node n1 to node n2,
     where f is a function taking the old value and returning
     the new one. If the graph did not contain the edge, it
     will be created."))

(defprotocol PNodeDataGraph
  "Functionality of a graph which stores data with its
   nodes."
  (node-data [g n]
    "Returns the data of the node n."))

(defprotocol PEditableNodeDataGraph
  "Functionality of a graph which stores editable data
   with its nodes."
  (update-node-data [g n f]
    "Updates the data of the node n, where f is a function
     taking the old value and returning the new one. If the
     graph did not contain the node, it will be added."))

(defprotocol PEdgeDataGraph
  "Functionality of a graph which stores data with its edges."
  (edge-data [g n1 n2]
    "Returns the data of the edge from node n1 to node n2."))

(defprotocol PEditableEdgeDataGraph
  "Functionality of a graph which stores editable data
   with its edges."
  (update-edge-data [g n1 n2 f]
    "Changes the data of the edge from node n1 to n2, where
     f is a function taking the old value and returning the
     new one. If the graph did not contain the edge, it will
     be added."))
