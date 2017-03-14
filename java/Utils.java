package network.wai.servlet;

import java.nio.ByteBuffer;
import eta.runtime.io.MemoryManager;

public class Utils {
    public static byte[] toByteArray(ByteBuffer ptr, int offset, int length) {
        ByteBuffer buf=ptr.duplicate();
        buf=MemoryManager.bufSetOffset(buf,offset);
        buf=MemoryManager.bufSetLimit(buf,length);
        byte[] b = new byte[buf.remaining()];
        buf.get(b);
        return b;
    }

}
