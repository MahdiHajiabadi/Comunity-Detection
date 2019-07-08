package org.jgrapht.demo;
import java.io.File;
import java.io.StringReader;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.builder.GraphTypeBuilder;
import org.jgrapht.io.EdgeProvider;
import org.jgrapht.io.GmlImporter;
import org.jgrapht.io.VertexProvider;
import org.jgrapht.util.SupplierUtil;
import org.jgrapht.*;
import java.io.*;
import java.util.*;
import java.lang.Math;
import org.jgrapht.graph.AsSubgraph;
public class App {
	int V; 
	int E;
	int com_num;
	int[][] truth;
	double[][] membership;
	double[][] S;
	double[][] F;
	double[][] I;
	double[][] W;
	double[][] beta;
	Graph<String, DefaultEdge> graph;
	HashMap<Integer,ArrayList<Integer>> hm;
	public App (String[] args) throws Exception {
		String basename = args[0];
		String com = args[1];
		com_num = Integer.parseInt(com);
		System.out.println(" Number of Community is: " + com_num);
		beta = new double[com_num][com_num];
		for (int i = 0 ; i < com_num ; i++)
			beta[i][i] = 3;
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
		membership = new double[V][com_num];
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
		S = new double[V][Feat.size()];

		for (int i = 0 ; i < V ; i++) {
			ArrayList<Integer> var = hm.get(i);
			for (int j = 0 ; j < var.size() ; j++)
				S[i][var.get(j)] = 1;
		}
		I = new double[com_num][com_num];
		W = new double[com_num][com_num];
		F = new double[V][com_num];
		F = S.clone();
		for (int i = 0 ; i < com_num ; i++){
			I[i][i] = 1;
			W[i][i] = 1;
		}
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
		System.out.println("Number of Nodes: " + graph.vertexSet().size() + " Number of Edges: "+ graph.edgeSet().size());
	}
	// ==========================================================================

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
		Integer[] indices = new Integer[V];
		for (int i = 0 ; i < V; i++)
			indices[i] = i;
        Arrays.sort(indices, (o1,o2) -> Double.compare(Conductance[o1],Conductance[o2]));
        counter = 0;
        int i = 0;
        boolean[] marker = new boolean[V];
        while ((counter<com_num) && (i < V)){
        	if (!marker[indices[i]]){
        		marker[indices[i]] = true;
        		Set<String> Neigh = Graphs.neighborSetOf(graph, String.valueOf(i));
        		for (String s2:Neigh){
        			membership[Integer.valueOf(s2)][counter] = 1;
        			marker[Integer.valueOf(s2)] = true;
        		}
        	}
        	i++;
        }
	}
	//==============================================================================
	public void Update_Membership(){
		double[][] Membership_difference = new double[V][com_num];
		double[] sum_total = add_columns(membership);
		for (String s: graph.vertexSet()){
			double[] source_membership = membership[Integer.valueOf(s)];
			double[] neighbor_membership_acc = new double[com_num];
			double[] source_times_beta = multiply(source_membership, beta);
			double result = 0;
			Set<String> Neigh = Graphs.neighborSetOf(graph,s);
			for (String s2: Neigh){
				result = result + dot(source_times_beta , membership[Integer.valueOf(s2)]);
				neighbor_membership_acc = add(neighbor_membership_acc,membership[Integer.valueOf(s2)]);
			}
			double Up = Math.exp(-result);
			double Down = 1 - Up;
			if (Down ==0) continue;
			double Factor = Up/Down;
			double[] neigh_times_beta = multiply(neighbor_membership_acc,beta);
			for (int i = 0 ; i < com_num ; i++) neigh_times_beta[i] = neigh_times_beta[i] * Factor;
			double[] non_neigh_membership_acc = subtract(sum_total , neighbor_membership_acc);
			double[] non_neigh_times_beta = multiply(non_neigh_membership_acc,beta);
			Membership_difference[Integer.valueOf(s)] = subtract(neigh_times_beta , non_neigh_times_beta);
			
			// System.out.println(neighbor_membership_acc[0]);
			// Going for geting the differences by attributes. 
			double[] node_att = multiply(membership[Integer.valueOf(s)] , W);
			for (int i = 0 ; i < com_num ; i++)
				node_att[i] = 1.0/(1 + Math.exp(-node_att[i]));
			double[] first_val = subtract(F[Integer.valueOf(s)] , node_att);
			double[] diff_att = multiply(first_val , W);
			Membership_difference[Integer.valueOf(s)] = add(Membership_difference[Integer.valueOf(s)] , diff_att);
			System.out.println(Membership_difference[Integer.valueOf(s)][0]);
		}
	}
	//=============================================================================
	 // return c = a - b
    public static double[][] subtract(double[][] a, double[][] b) {
        int m = a.length;
        int n = a[0].length;
        double[][] c = new double[m][n];
        for (int i = 0; i < m; i++)
            for (int j = 0; j < n; j++)
                c[i][j] = a[i][j] - b[i][j];
        return c;
    }
    //==========================================================================
    public static double[] subtract(double[] a, double[] b) {
        int m = a.length;
        double[] c = new double[m];
        for (int i = 0; i < m; i++)
            c[i] = a[i] - b[i];
        return c;
    }
	//=============================================================================
	public double dot(double[] a, double[] b){
		if (a.length != b.length) throw new RuntimeException("Illegal vector dimensions.");
		double result = 0;
		for (int i = 0 ; i < a.length ; i++)
			result = result + a[i] * b[i];
		return result;
	}
	//==================================================================
	    // return c = a * b
    public static double[][] multiply(double[][] a, double[][] b) {
        int m1 = a.length;
        int n1 = a[0].length;
        int m2 = b.length;
        int n2 = b[0].length;
        if (n1 != m2) throw new RuntimeException("Illegal matrix dimensions.");
        double[][] c = new double[m1][n2];
        for (int i = 0; i < m1; i++)
            for (int j = 0; j < n2; j++)
                for (int k = 0; k < n1; k++)
                    c[i][j] += a[i][k] * b[k][j];
        return c;
    }
	//================================================================================
    // matrix-vector multiplication (y = A * x)
    public static double[] multiply(double[][] a, double[] x) {
        int m = a.length;
        int n = a[0].length;
        if (x.length != n) throw new RuntimeException("Illegal matrix dimensions.");
        double[] y = new double[m];
        for (int i = 0; i < m; i++)
            for (int j = 0; j < n; j++)
                y[i] += a[i][j] * x[j];
        return y;
    }
    //========================================================================
    // vector-matrix multiplication (y = x^T A)
    public static double[] multiply(double[] x, double[][] a) {
        int m = a.length;
        int n = a[0].length;
        if (x.length != m) throw new RuntimeException("Illegal matrix dimensions.");
        double[] y = new double[n];
        for (int j = 0; j < n; j++)
            for (int i = 0; i < m; i++)
                y[j] += a[i][j] * x[i];
        return y;
    }
//=================================================================================
    public static double[] add_columns(double[][] a){
    	int m = a.length;
    	int n = a[0].length;
    	double[] y = new double[n];
    	for (int i = 0 ; i < m ; i++){
    		for (int j = 0 ; j < n ; j++)
    			y[j] = y[j] + a[i][j];
    	}
    	return y;
    }
    //==========================================================================
    public static double[] add(double[] a, double[] b){
    	if (a.length != b.length) throw new RuntimeException("Illegal vector dimensions.");
    	double[] y = new double[a.length];
    	for (int i = 0 ; i < a.length ; i++)
    		y[i] = a[i] + b[i];
    	return y;
    }
    //==========================================================================
	public static void main(String[] args) throws Exception {
		String basename = args[0];
		App temp = new App(args);
		temp.Intialize_Conductance();		
		temp.Update_Membership();
	}
}