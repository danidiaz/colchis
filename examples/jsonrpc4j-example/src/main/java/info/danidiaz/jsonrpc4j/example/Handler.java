package info.danidiaz.jsonrpc4j.example;

import com.fasterxml.jackson.databind.*; // JsonNode
import com.fasterxml.jackson.databind.node.JsonNodeFactory;

public class Handler implements HandlerInterface {
	
	public JsonNode doSomething(String args) {
		JsonNodeFactory factory = JsonNodeFactory.instance;	
		return factory.objectNode().put("doubled", args + args);
	}
	
	public int plusone(int i) {
		return i + 1;
	}
}
