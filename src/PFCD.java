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
import org.jgrapht.io.CSVImporter;
import java.io.*;
import java.util.*;
import java.lang.Math;
import org.jgrapht.graph.AsSubgraph;
public class PFCD {
	int V; 
	double sparcity;
	double alpha;
	int E;
	int com_num;
	int[][] truth;
	double[][] membership;
	double[][] S;
	double[][] F;
	double[][] I;
	double[][] W;
	double[][] beta;
	int[][] M;
	int[][] Confusion;
	Graph<String, DefaultEdge> graph;
	HashMap<Integer,ArrayList<Integer>> hm;
	public PFCD (String[] args) throws Exception {
		alpha = 0.01;
		String basename = args[0];
		String com = args[1];
		com_num = Integer.parseInt(com);
		System.out.println(" Number of Community is: " + com_num);
		beta = new double[com_num][com_num];
		for (int i = 0 ; i < com_num ; i++)
			for(int j = 0 ; j < com_num ; j++)
				if (i ==j)
					beta[i][i] = 0.3;
				else
					beta[i][j] = 0.1;
		// File file = new File(basename);
		// File file = new File("/home/khsh/MetaData/DataSet/WeddellSea_network/WeddellSea_Feeding_type.gml");
		// File file = new File("/home/khsh/MetaData/Codes/Inference/Lawyer.gml");
		// File file = new File("/home/khsh/MetaData/Codes/Inference/WorldTrade.gml");
		// File file = new File("/home/khsh/MetaData/Codes/Inference/Rice.gml");
		// File file = new File("/home/khsh/MetaData/Codes/Inference/Elizaveth1-2-4.edgelist");
		File file = new File("/home/khsh/MetaData/Codes/Inference/EdgeLawyer");
	  	VertexProvider<String> vertexProvider = (label, attributes) -> label;
	    EdgeProvider<String, DefaultEdge> edgeProvider = (from, to, label, attributes) -> new DefaultEdge();
	    CSVImporter<String, DefaultEdge> csvImporter = new CSVImporter<>(vertexProvider, edgeProvider);
	    // csvImporter.setFormat(CSVFormat.EDGE_LIST);
	    Graph<String, DefaultEdge> graph = GraphTypeBuilder
				.undirected()
				.allowingMultipleEdges(false)
				.allowingSelfLoops(false)
				.vertexSupplier(SupplierUtil.createStringSupplier())
				.edgeSupplier(SupplierUtil.createDefaultEdgeSupplier())
				.buildGraph();	
	   	byte[] fileBytes = Files.readAllBytes(Paths.get(file.getAbsolutePath()));
		StringReader sr = new StringReader(new String(fileBytes, "UTF-8"));	
	      csvImporter.importGraph(graph, sr);
	    System.out.println("Number of Vertices Are: " + graph.vertexSet().size());
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
        		Set<String> Neigh = Graphs.neighborSetOf(graph, String.valueOf(indices[i]));
        		for (String s2:Neigh){
        			membership[Integer.valueOf(s2)][counter] = 1;
        			marker[Integer.valueOf(s2)] = true;
        		}
        		counter++;
        	}
        	i++;
        }
        // double[] tt = add_columns(membership);
	}
	//==============================================================================
	public void Update_Membership(){
		double[][] Membership_difference = new double[V][com_num];
		double[] sum_total = add_columns(membership);
		double[][] deltaI = new double[com_num][com_num];
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
			//====================================================== Important Line
			double[] non_neigh_membership_acc = subtract(sum_total , neighbor_membership_acc);
			double[] non_neigh_times_beta = multiply(non_neigh_membership_acc,beta);
			Membership_difference[Integer.valueOf(s)] = subtract(neigh_times_beta , non_neigh_times_beta ,  Neigh.size());
			System.out.println(" Structural inference is: " + Membership_difference[Integer.valueOf(s)][0]);
			double[] node_att = multiply(membership[Integer.valueOf(s)] , W);
			for (int i = 0 ; i < com_num ; i++)
				node_att[i] = 1.0/(1 + Math.exp(-node_att[i]));
			double[] first_val = subtract(F[Integer.valueOf(s)] , node_att);
			double[] diff_att = multiply(first_val , W);
			System.out.println(" Attributal inference is: " + diff_att[0]);
			Membership_difference[Integer.valueOf(s)] = add(Membership_difference[Integer.valueOf(s)] , diff_att);
			// System.out.println(Membership_difference[Integer.valueOf(s)][0]);
			//=============================== Updating the Parameter I
			double[] S_u = S[Integer.valueOf(s)];
			double[] S_u_times_I = multiply(S_u, I);
			Factor = 0 ;
			for (int i = 0 ; i < S_u_times_I.length ; i++)
				Factor = Factor + S_u_times_I[i];
			Up = Math.exp(-Factor);
			Down = Math.pow(1 + Math.exp(-Factor), 2);
			Factor = Up/Down;
			for (int i = 0 ; i < S_u.length ; i++)
				for (int j = 0 ; j < com_num ; j++)
				deltaI[i][j] = deltaI[i][j] + S_u[i] * Factor * Membership_difference[Integer.valueOf(s)][j];
		}
		for (String s:graph.vertexSet())
			for(int j = 0 ; j < com_num ; j++){
				membership[Integer.valueOf(s)][j] = membership[Integer.valueOf(s)][j] + alpha * Membership_difference[Integer.valueOf(s)][j];
				if (membership[Integer.valueOf(s)][j] < 0) membership[Integer.valueOf(s)][j] = 0;
			}
			for (int i = 0 ; i < com_num ; i++)
				for(int j = 0 ; j < com_num ; j++){
					I[i][j] = I[i][j] + alpha * deltaI[i][j];
			}
	}
	//=============================================================================
	public void update_params(){
		double[][] deltaB = new double[com_num][com_num];
		alpha = 0.01;
		double[][] deltaW = new double[com_num][com_num];
		double[] sum_total = add_columns(membership);
		for (String s: graph.vertexSet()){
			double[] M_W = multiply(membership[Integer.valueOf(s)] , W);
			for (int i = 0 ; i < M_W.length ; i++){
				M_W[i] =  F[Integer.valueOf(s)][i] - 1/(1 + Math.exp(-M_W[i]));
			}
			for (int i = 0 ; i < M_W.length ; i++)
				for (int j = 0 ; j < com_num ; j++)
					deltaW[i][j] = M_W[i] * membership[Integer.valueOf(s)][j];
			for (int i = 0 ; i < com_num ; i++)
				for (int j = 0 ; j < com_num ; j++)
					W[i][j] = W[i][j] + alpha * deltaW[i][j];
		//============================== Updating Parameters Beta
			double[] source_membership = membership[Integer.valueOf(s)];
			double[] neighbor_membership_acc = new double[com_num];
			double[] source_times_beta = multiply(source_membership, beta);
			double result = 0;
			Set<String> Neigh = Graphs.neighborSetOf(graph,s);
			for (String s2: Neigh){
				result = result + dot(source_times_beta , membership[Integer.valueOf(s2)]);
				neighbor_membership_acc = add(neighbor_membership_acc,membership[Integer.valueOf(s2)]);
			}
			double Factor = dot(source_times_beta , neighbor_membership_acc);
			double Up = Math.exp(-Factor);
			double Down = 1 - Math.exp(-Factor);
			if (Down==0) continue;
			for (int i = 0 ; i < com_num ; i++)
				for (int j = 0 ; j < com_num ; j++)
					deltaB[i][j] = deltaB[i][j] - source_membership[i] * neighbor_membership_acc[j] * Up/Down;
			double[] non_neigh_membership_acc = subtract(sum_total , neighbor_membership_acc);
			for (int i = 0 ; i < com_num ; i++)
				for (int j = 0 ; j < com_num ; j++)
					deltaB[i][j] = deltaB[i][j] - source_membership[i] * non_neigh_membership_acc[j];
		}
		// for (int i = 0 ; i < com_num ; i++)
		// 	for (int j = 0 ; j < com_num ; j++)
		// 		beta[i][j] = beta[i][j] + alpha * deltaB[i][j];

	}
	//=================================================================================
	public void deterministic_membership(){
		double Threshold = 0.5;
		M = new int[V][com_num];
		for(int i = 0 ; i < V ; i++){
			int idx = 0;
			double max = 0;
			for (int j = 0 ; j < com_num ; j++)
			{
				if(membership[i][j]> max){
					// M[i][j] = 1;
					max = membership[i][j];
					idx = j;
				}
			}
			M[i][idx] = 1;
		}
	}
	//===========================================================================
	public void matching()
	{
		int[][] temp_M = new int[V][com_num];
		boolean[] marker = new boolean[com_num];
		for (int i = 0 ; i < com_num ; i++){
			int[] common = new int[com_num];
			int max = 0;
			for (int j = 0 ; j < com_num ; j++){
				if (marker[j]==true) continue;
				for(int k = 0 ; k < V ; k++){
					if ((M[k][i]==1) && (truth[k][j]==1))
						 common[j]++;
				}
			}
			Integer[] G = new Integer[com_num];
			for(int ttt = 0; ttt < com_num; ttt++) G[ttt] = ttt;
			Arrays.sort(G, (o1,o2) -> Integer.compare(common[o2],common[o1]));
			// System.out.println(G[0] + " Second Element is: " + G[1]);
			int counter = 0;
			int idx = G[counter];
			while(marker[idx]==true){
				counter++;
				idx = G[counter];
			}
			marker[idx] = true;
			for (int vertex = 0 ; vertex < V ; vertex++){
				int swap = M[vertex][i];
				M[vertex][i] = M[vertex][idx];
				M[vertex][idx] = swap;
			}
		}
	}
	//=================================================================================
	public double F_Meausre()
	{
		Confusion = new int[com_num][com_num];
		double sum_val = 0;
		for(int i = 0 ; i < V ; i++)
		{
			int idx_truth = 0;
			int idx_prediction = 0;
			for (int j = 0 ; j < com_num ; j++){
				if (truth[i][j]==1)
					idx_truth = j;
				if(M[i][j]==1)
					idx_prediction = j;
				Confusion[idx_truth][idx_prediction]++;
			}
		}
		double precision = 0; 
		double recall = 0;
		double[] F_Score = new double[com_num];
		for (int i = 0 ; i < com_num ; i++){
			int sum_col = 0;
			int sum_row = 0;
			for (int j = 0 ; j < com_num ; j++)
			{
				sum_row = sum_row + Confusion[i][j];
				sum_col = sum_col + Confusion[j][i];
			}
			precision = (Confusion[i][i] * 1.0) / (sum_row * 1.0);
			recall = (Confusion[i][i] * 1.0) / (sum_col * 1.0);
			F_Score[i] = (2 * precision * recall)/(precision + recall);
			sum_val = F_Score[i] + sum_val;
		}
		return sum_val/(com_num * 1.0);
	}
	//========================================================================= 
	//Entropy
	public double Entropy(int[][] Labels)
	{
		int[] sum = add_columns(Labels);
		double entropy = 0;
		for (int i = 0 ; i < com_num ; i++){
			double prob = (sum[i]*1.0)/(V * 1.0);
			entropy = entropy + (-1 * prob) * Math.log(prob)/Math.log(2);
		}
		return entropy;
	}
	//===============================================================================
	public double conditional_entropy(){
		int[] sum_col = add_columns(Confusion);
		double  result = 0; 
		for (int i = 0 ; i < com_num ; i++)
		{
			double temp = 0;
			for (int j = 0 ; j < com_num ; j++){
				double prob = (Confusion[j][i] * 1.0)/(sum_col[i] * 1.0);
				if(prob==0) continue;
				temp = temp + prob * Math.log(prob)/Math.log(2);

			}
			result = result + (-sum_col[i]* 1.0) / (V * 1.0) * temp;
		}
		return result;
	}
	//================================================================================
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
    // return c = a - b
    public  double[] subtract(double[] a, double[] b,int sparcity) {
        int m = a.length;
        double factor1 = 1.0/(1 * sparcity);
        double factor2 = 1.0/((V - sparcity)* 1.0);
        double[] c = new double[m];
        for (int i = 0; i < m; i++)
            c[i] = a[i] * factor1 - b[i] * factor2;
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

    public static int[] add_columns(int[][] a){
    	int m = a.length;
    	int n = a[0].length;
    	int[] y = new int[n];
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
    //==============================
    public static double[] add(double[] a, double[] b , double sparcity){
    	if (a.length != b.length) throw new RuntimeException("Illegal vector dimensions.");
    	double[] y = new double[a.length];
    	for (int i = 0 ; i < a.length ; i++)
    		y[i] = a[i] + b[i] * sparcity;
    	return y;
    }
    //==========================================================================
	public static void main(String[] args) throws Exception {
		String basename = args[0];
		PFCD temp = new PFCD(args);
		// temp.membership = temp.S.clone();
		// // temp.Intialize_Conductance();
		// System.out.println();
		// for (int iter = 0 ; iter < 11; iter++){
		// 	temp.Update_Membership();
		// 	temp.update_params();
		// 	System.out.println(iter);
		// }
		// temp.deterministic_membership();
		// int[] res = temp.add_columns(temp.M);
		// for (int ii = 0 ; ii < res.length ; ii++)
		// 		System.out.print(res[ii] + "  ");
		// temp.matching();
		// double f_measure = temp.F_Meausre();
		// System.out.println("F_Measure value is: " + f_measure);
		// double H_Y = temp.Entropy(temp.truth);
		// System.out.println(" H(Y): " + H_Y);
		// double H_C = temp.Entropy(temp.M);
		// System.out.println(" H(C): " + H_C);
		// double I_Y_C = temp.conditional_entropy();
		// I_Y_C = H_Y - I_Y_C;
		// System.out.println(" I(Y:C): " + I_Y_C);
		// double NMI = 2 * I_Y_C/(H_Y + H_C);
		// System.out.println("NMI is: " + NMI);
	}
}