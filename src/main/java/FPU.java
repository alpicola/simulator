package cpuex4;

import java.io.*;

class FPU {
    private boolean dumpEnable;
    private PrintWriter faddOut;
    private PrintWriter fsubOut;
    private PrintWriter fmulOut;
    private PrintWriter fdivOut;
    private PrintWriter finvOut;
    private PrintWriter fsqrtOut;

    FPU(boolean dump) throws FileNotFoundException {
        dumpEnable = dump;

        if (dumpEnable) {
            faddOut = new PrintWriter("fadd");
            fsubOut = new PrintWriter("fsub");
            fmulOut = new PrintWriter("fmul");
            fdivOut = new PrintWriter("fdiv");
            finvOut = new PrintWriter("finv");
            fsqrtOut = new PrintWriter("fsqrt");
        }
    }

    // TODO: These implementation should be functionally equivalent to FPU's 

    float fadd(float a, float b) throws IOException {
        float r = a + b;

        if (dumpEnable) {
            faddOut.printf("0x%8x 0x%8x 0x%8x%n", a, b, r);
        }

        return r;
    }

    float fsub(float a, float b) throws IOException {
        float r = a - b;

        if (dumpEnable) {
            fsubOut.printf("0x%8x 0x%8x 0x%8x%n", a, b, r);
        }

        return r;
    }

    float fmul(float a, float b) throws IOException {
        float r = a * b;

        if (dumpEnable) {
            fmulOut.printf("0x%8x 0x%8x 0x%8x%n", a, b, r);
        }

        return r;
    }

    float fdiv(float a, float b) throws IOException {
        float r = a / b;

        if (dumpEnable) {
            fdivOut.printf("0x%8x 0x%8x 0x%8x%n", a, b, r);
        }

        return r;
    }

    float finv(float a) throws IOException {
        float r = 1.0f / a;

        if (dumpEnable) {
            finvOut.printf("0x%8x 0x%8x%n", a, r);
        }

        return r;
    }

    float fsqrt(float a) throws IOException {
        float r = (float) Math.sqrt((double) a);

        if (dumpEnable) {
            fsqrtOut.printf("0x%8x 0x%8x%n", a, r);
        }

        return r;
    }
}
