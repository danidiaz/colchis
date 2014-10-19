package info.danidiaz.jsonrpc4j.example;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.ServerSocket;

import com.googlecode.jsonrpc4j.JsonRpcServer;
import com.googlecode.jsonrpc4j.StreamServer;
// JsonNode

public class Main 
{
    private final static int DEFAULT_PORT = 26060;   

    // private final ServerSocket serverSocket;
    private ByteArrayOutputStream imageBuffer = new ByteArrayOutputStream();
    
    // http://docs.oracle.com/javase/6/docs/api/java/lang/instrument/package-summary.html
    public static void main(String argv []) {
    	// https://github.com/briandilley/jsonrpc4j
    	JsonRpcServer jsonRpcServer = new JsonRpcServer(new Handler(), HandlerInterface.class);
    	
    	ServerSocket serverSocket;
		try {
			serverSocket = new ServerSocket(DEFAULT_PORT);
			int maxThreads = 2;
			StreamServer streamServer = new StreamServer(jsonRpcServer, maxThreads, serverSocket);
			streamServer.start();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
    }

}
