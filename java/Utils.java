package network.wai.servlet;

import java.nio.ByteBuffer;
import eta.runtime.io.MemoryManager;
import java.io.InputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
    
public class Utils {
    public static byte[] toByteArray(ByteBuffer ptr, int offset, int length) {
        ByteBuffer buf=ptr.duplicate();
        buf=MemoryManager.bufSetOffset(buf,offset);
        buf=MemoryManager.bufSetLimit(buf,length);
        byte[] b = new byte[buf.remaining()];
        buf.get(b);
        return b;
    }

    public static ByteBuffer toByteBuffer(InputStream is)
      throws IOException {
        byte[] bytes=toByteArray(is);
        return ByteBuffer.wrap(bytes);
    }
    // from apache.commons.io.IOUtils
    private static final int EOF = -1;
    private static final int DEFAULT_BUFFER_SIZE = 1024 * 4;
    public static byte[] toByteArray (InputStream input) throws
        IOException {
        byte[] buffer=new byte[DEFAULT_BUFFER_SIZE];
        try (final ByteArrayOutputStream output =
             new ByteArrayOutputStream()) {
            long count = 0;
            int n;
            while (EOF != (n = input.read(buffer))) {
                output.write(buffer, 0, n);
                count += n;
            }
            return output.toByteArray();
        } 
    }
    public static int size(ByteBuffer buf) {
        return buf.remaining();
    }
}
