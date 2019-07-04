package org.jgrapht.demo;
import java.io.File;
import java.io.StringReader;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashSet;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.builder.GraphTypeBuilder;
import java.util.HashMap;
import org.jgrapht.io.EdgeProvider;
import org.jgrapht.io.GmlImporter;
import org.jgrapht.io.VertexProvider;
import org.jgrapht.util.SupplierUtil;
import org.jgrapht.*;
import org.jgrapht.graph.*;
import org.jgrapht.graph.AbstractGraph.*;
// import org.jgrapht.io.*;
// import org.jgrapht.traverse.*;
import java.io.*;
import java.net.*;
import java.util.*;


// import org.jgrapht.alg.util.VertexDegreeComparator;
import org.jgrapht.graph.AsSubgraph;
// import org.jgrapht.graph.UndirectedSubgraph;




public class App {
	int V; 
	int E;
	int com_num;
	int[][] truth;
	int[][] membership;
	Graph<String, DefaultEdge> graph;
	HashMap<Integer,ArrayList<Integer>> hm;
	public App (String[] args) throws Exception {
		String basename = args[0];
		String com = args[1];
		com_num = Integer.parseInt(com);
		System.out.println(" Number of Community is: " + com_num);
		// File file = new File(basename);
		File file = new File("/home/khsh/MetaData/DataSet/WeddellSea_network/WeddellSea_Environment.gml");
			Graph<String, DefaultEdge> graph_t = GraphTypeBuilder
				.undirected()
				.allowingMultipleEdges(false)
				.allowingSelfLoops(false)
				.vertexSupplier(SupplierUtil.createStringSupplier())
				.edgeSupplier(SupplierUtil.createDefaultEdgeSupplier())
				.buildGraph();
		
		VertexProvider<String> vp = (id, attributes) ->{ return id; 
	};

		VertexProvider<String> vp2 = (id, attributes) -> {return id + "," + attributes.get("label").toString() + ",";
    };
		EdgeProvider<String, DefaultEdge> ep = (from, to, label, attributes) -> new DefaultEdge();
		GmlImporter<String, DefaultEdge> importer = new GmlImporter<>(vp2, ep);
		byte[] fileBytes = Files.readAllBytes(Paths.get(file.getAbsolutePath()));
		StringReader sr = new StringReader(new String(fileBytes, "UTF-8"));	
		importer.importGraph(graph_t, sr);
		V = graph_t.vertexSet().size();
		HashMap<String,Integer> Feat = new HashMap<String,Integer>();
		int counter = 0;
		hm = new HashMap<Integer, ArrayList<Integer>>();
		int counter_feat = 0;
		for (String s: graph_t.vertexSet()){
			int st_pos = 0;
			int idx = s.indexOf(',', st_pos);
			ArrayList<Integer> temp = new ArrayList<Integer>();
			st_pos = idx+1;
			while(s.indexOf(',', st_pos)!=-1)
			{
				idx = s.indexOf(',', st_pos);
				String current_str = s.substring(st_pos,idx);
				if (counter_feat==0){
					temp.add(counter_feat);
					Feat.put(current_str,counter_feat++);
					continue;
				}
				if(Feat.containsKey(current_str))
					temp.add(Feat.get(current_str));
				else
				{
					temp.add(counter_feat);
					Feat.put(current_str,counter_feat++);

				}
				st_pos = idx+1;
			}
			hm.put(counter,temp);
			counter++;
		}
		System.out.println(hm.size() + " Number of Vertices: " + V + " Size of the HashMap is: " + Feat.size());
		graph = GraphTypeBuilder
				.undirected()
				.allowingMultipleEdges(false)
				.allowingSelfLoops(false)
				.vertexSupplier(SupplierUtil.createStringSupplier())
				.edgeSupplier(SupplierUtil.createDefaultEdgeSupplier())
				.buildGraph();
		importer = new GmlImporter<>(vp, ep);
		fileBytes = Files.readAllBytes(Paths.get(file.getAbsolutePath()));
		sr = new StringReader(new String(fileBytes, "UTF-8"));	
		importer.importGraph(graph, sr);
		System.out.println("Number of Nodes: " + graph.vertexSet().size() + " Nimber of Edges: "+ graph.edgeSet().size());
	}
	public void Intialize_Conductance(){
		double[] Conductance = new double[V];
		int counter = 0;
		for (String s: graph.vertexSet()){
			int total_degree = 0;
			Set<String> Neigh = Graphs.neighborSetOf(graph,s);
			Graph<String,DefaultEdge> S = new AsSubgraph<String,DefaultEdge>(graph, Neigh);
			for (String s2: Neigh){
				total_degree = total_degree + graph.degreeOf(s2);
			}
			int cut = total_degree - S.edgeSet().size();
			int min_edges = total_degree;
			if (graph.edgeSet().size() * 2 - total_degree < min_edges)
				min_edges = graph.edgeSet().size() * 2 - total_degree;
			Conductance[counter++] = cut/(min_edges * 1.0);
		}
		for (int i = 0 ; i < V ; i++)
			System.out.print(Conductance[i] + "\t");
	}
	public static void main(String[] args) throws Exception {
		String basename = args[0];
		App temp = new App(args);
		temp.Intialize_Conductance();
		// Graph<String, DefaultEdge> graph = GraphTypeBuilder
		// 		.undirected()
		// 		.allowingMultipleEdges(false)
		// 		.allowingSelfLoops(false)
		// 		.vertexSupplier(SupplierUtil.createStringSupplier())
		// 		.edgeSupplier(SupplierUtil.createDefaultEdgeSupplier())
		// 		.buildGraph();
		
		// VertexProvider<String> vp = (id, attributes) -> {
		// 	System.out.println("Creating node " + id);
		// 	for(String key: attributes.keySet()) { 
		// 		System.out.println("key: " + key + ", value: " + attributes.get(key));
		// 	}
		// 	return id;
		// };
		// VertexProvider<String> vp = (id, attributes) -> id; 
		// {
		// System.out.println("Creating node " + id);
		// for(String key: attributes.keySet()) { 
		// 	System.out.println("key: " + key + ", value: " + attributes.get(key));
		// }
		// return id;
		// };

		// EdgeProvider<String, DefaultEdge> ep = (from, to, label, attributes) -> new DefaultEdge();
		// GmlImporter<String, DefaultEdge> importer = new GmlImporter<>(vp, ep);
		// // File file = new File("/home/khsh/Dropbox/Hajiabadi/working papers/Metadata_SecondWork/Gradient-Based Implementaion/MetaData/Inference/WorldTrade.gml");
		// File file = new File("/home/khsh/MetaData/DataSet/WeddellSea_network/WeddellSea_Environment.gml");
		// byte[] fileBytes = Files.readAllBytes(Paths.get(file.getAbsolutePath()));
		// StringReader sr = new StringReader(new String(fileBytes, "UTF-8"));		
		// importer.importGraph(graph, sr);
		// System.out.println("The Degree of Node " + 0 + " is: " + graph.degreeOf("341"));
		
	}
	
}