package cpuex4;

import java.io.*;

class FPU {
    private boolean dumpEnable;
    private DataOutputStream faddOut;
    private DataOutputStream fsubOut;
    private DataOutputStream fmulOut;
    private DataOutputStream fdivOut;
    private DataOutputStream finvOut;
    private DataOutputStream fsqrtOut;

    FPU(boolean dump) throws FileNotFoundException {
        dumpEnable = dump;

        if (dumpEnable) {
            faddOut = new DataOutputStream(new BufferedOutputStream(new FileOutputStream("fadd")));
            fsubOut = new DataOutputStream(new BufferedOutputStream(new FileOutputStream("fsub")));
            fmulOut = new DataOutputStream(new BufferedOutputStream(new FileOutputStream("fmul")));
            fdivOut = new DataOutputStream(new BufferedOutputStream(new FileOutputStream("fdiv")));
            finvOut = new DataOutputStream(new BufferedOutputStream(new FileOutputStream("finv")));
            fsqrtOut = new DataOutputStream(new BufferedOutputStream(new FileOutputStream("fsqrt")));
        }
    }

    // TODO: These implementation should be functionally equivalent to FPU's 

    float fadd(float a, float b) throws IOException {
        float r = a + b;

        if (dumpEnable) {
            faddOut.writeFloat(a);
            faddOut.writeFloat(b);
            faddOut.writeFloat(r);
        }

        return r;
    }

    float fsub(float a, float b) throws IOException {
        float r = a - b;

        if (dumpEnable) {
            fsubOut.writeFloat(a);
            fsubOut.writeFloat(b);
            fsubOut.writeFloat(r);
        }

        return r;
    }

    float fmul(float a, float b) throws IOException {
        float r = a * b;

        if (dumpEnable) {
            fmulOut.writeFloat(a);
            fmulOut.writeFloat(b);
            fmulOut.writeFloat(r);
        }

        return r;
    }

    float fdiv(float a, float b) throws IOException {
        float r = a / b;

        if (dumpEnable) {
            fdivOut.writeFloat(a);
            fdivOut.writeFloat(b);
            fdivOut.writeFloat(r);
        }

        return r;
    }

    float finv(float a) throws IOException {
        float r = 1.0f / a;

        if (dumpEnable) {
            finvOut.writeFloat(a);
            finvOut.writeFloat(r);
        }

        return r;
    }

    float fsqrt(float a) throws IOException {
        float r = (float) Math.sqrt((double) a);

        if (dumpEnable) {
            fsqrtOut.writeFloat(a);
            fsqrtOut.writeFloat(r);
        }

        return r;
    }
}
