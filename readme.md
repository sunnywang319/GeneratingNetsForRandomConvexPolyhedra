##Abstract

This project is essentially a visualization of Shephard's conjecture, which states that every convex polyhedron admits a self-nonoverlapping unfolding. Currently, there exists unfolding data for several predefined polyhedra in the Wolfram Language, and my goal is to create a function that can do this for any random convex polyhedron. I chose this project because of my love for origami and interest in 3D modeling.

##Generating a graph for the faces of the polyhedron

The first step was to create a graph that captures the relationship between faces of the polyhedron so that it could be used later to generate the net. The built-in function DualPolyhedron converts the polyhedron to one where each vertex corresponds to a face on the original. I then extracted the vertices of the dual polyhedron and partitioned and sorted them, allowing me to create a graph using those vertices.

    ClearAll[polyhedronfacegraph];
    
    polyhedronfacegraph[polyhedron_]:= 
    Block[{dualpolyhedron, vertexlist, vertexpairings, sortedvertices},
    
    	 dualpolyhedron = DualPolyhedron[polyhedron];  (* Generate the dual polyhedron *)
         vertexlist = dualpolyhedron[[2]];   
         
         vertexpairings = Flatten[Table[
         Append[
         Partition[vertexlist[[n]], 2, 1], (* Partitions into groups of two *)
         {Last[vertexlist[[n]]], First[vertexlist[[n]]]}],  (* Appends the case where the last should be connected to the first *)
         {n, 1, Length[vertexlist]}], 
         1];
         
         sortedvertices = Sort /@ vertexpairings // DeleteDuplicates; (* Deletes duplicates by sorting them first *)
    
    Graph[UndirectedEdge@@@sortedvertices,VertexLabels -> "Name"] (* Graphs the sorted vertices and labels it *)
    ]
[PUT IMAGE HERE]

##Generating Spanning Trees 
I then used the graph to generate different paths in which a polyhedron could unfold. One way to do this is by using a spanning tree, a tree generated from a graph that retains the same amount of vertices while having the minimum amount of edges. This essentially creates a simple version of what the final net should look like. Each vertex represents a face, and connections between them signify that they are adjacent. It is important to note that not every spanning tree will correspond to a non overlapping net, which is why I generate a spanning tree from every possible vertex.
    generatetrees[graph_] := Table[FindSpanningTree[{graph, n}], {n, 1, VertexCount[graph]}]
