package cpuex4;

import java.io.*;
import java.nio.*;

class BinReader {
    private InputStream in;
    private ByteOrder order = ByteOrder.LITTLE_ENDIAN;
    private ByteBuffer buffer; 
    private byte[] array;

    BinReader(InputStream in) {
        this.in = in;
        buffer = ByteBuffer.allocate(4);
        array = buffer.array();
        buffer.order(order);
    }

    int readInt() throws IOException {
        in.read(array, 0, 4);
        return buffer.getInt(0);
    }

    byte readByte() throws IOException {
        in.read(array, 0, 1);
        return array[0];
    }

    float readFloat() throws IOException {
        in.read(array, 0, 4);
        return buffer.getFloat(0);
    }

    void close() throws IOException {
        in.close();
    }

}
