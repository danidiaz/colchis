package info.danidiaz.jsonrpc4j.example;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;

public interface HandlerInterface {
	public JsonNode doSomething(String args);
}
