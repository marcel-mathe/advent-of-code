use petgraph::graph::Graph;
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = &args[1];

    let contents = fs::read_to_string(path).expect("Something went wrong reading the file");

    day09p01(&contents);
    day09p02(&contents);
}

fn day09p01(_contents: &String) {
    let nodes = vec![];
    let edges = vec![];
    let graph = Graph::new();

    // London -> Dublin
    nodes.push(graph.add_node("London"));
    nodes.push(graph.add_node("Dublin"));
    graph.update_edge(nodes[0], nodes[1], 464);

    // London -> Belfast
    nodes.push(graph.add_node("Belfast"));
    graph.update_edge(nodes[0], nodes[2], 518);

    // Dublin -> Belfast
    graph.update_edge(nodes[1], nodes[2], 141);
}

fn day09p02(_contents: &String) {}
