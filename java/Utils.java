package network.wai.servlet;

import java.nio.ByteBuffer;
import eta.runtime.io.MemoryManager;
import java.io.InputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

public class Utils {

    public static byte[] toByteArray(long ptr, int offset, int length) {
        ByteBuffer buf = MemoryManager.getBoundedBuffer(ptr);
        buf.position(buf.position() + offset);
        buf.limit(buf.position() + length);
        byte[] b = new byte[buf.remaining()];
        buf.get(b);
        return b;
    }

    public static long toByteBuffer(InputStream is)
      throws IOException {
        byte[] bytes=toByteArray(is);
        long address = MemoryManager.allocateBuffer(bytes.length,true);
        ByteBuffer buf = MemoryManager.getBoundedBuffer(address);
        buf.put(bytes);
        buf.clear();
        return address;
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

    public static int size(long address) {
        ByteBuffer buf = MemoryManager.getBoundedBuffer(address);
        return size(buf);
    }
    
    public static int size(ByteBuffer buf) {
        return buf.remaining();
    }
}
