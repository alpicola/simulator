package cpuex4;

import java.io.*;
import java.nio.*;

class BinWriter {
    private OutputStream out;
    private ByteOrder order = ByteOrder.LITTLE_ENDIAN;
    private ByteBuffer buffer; 
    private byte[] array;

    BinWriter(OutputStream out) {
        this.out = out;
        buffer = ByteBuffer.allocate(4);
        array = buffer.array();
        buffer.order(order);
    }

    void writeInt(int v) throws IOException {
        buffer.putInt(0, v);
        out.write(array, 0, 4);
    }

    void writeByte(byte v) throws IOException {
        array[0] = v;
        out.write(array, 0, 1);
    }

    void writeFloat(float v) throws IOException {
        buffer.putFloat(0, v);
        out.write(array, 0, 4);
    }

    void close() throws IOException {
        out.close();
    }

}
