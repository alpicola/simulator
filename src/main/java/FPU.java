package cpuex4;

import java.util.*;

class FPU {
    static boolean loggingEnable;

    static List<Float> faddLog = new ArrayList<Float>();
    static List<Float> fsubLog = new ArrayList<Float>();
    static List<Float> fmulLog = new ArrayList<Float>();
    static List<Float> fdivLog = new ArrayList<Float>();
    static List<Float> finvLog = new ArrayList<Float>();
    static List<Float> fsqrtLog = new ArrayList<Float>();

    // TODO: These implementation should be functionally equivalent to FPU's 

    static float fadd(float a, float b) {
        float r = a + b;

        if (loggingEnable) {
            faddLog.add(a);
            faddLog.add(b);
            faddLog.add(r);
        }

        return r;
    }

    static float fsub(float a, float b) {
        float r = a - b;

        if (loggingEnable) {
            fsubLog.add(a);
            fsubLog.add(b);
            fsubLog.add(r);
        }

        return r;
    }

    static float fmul(float a, float b) {
        float r = a * b;

        if (loggingEnable) {
            fmulLog.add(a);
            fmulLog.add(b);
            fmulLog.add(r);
        }

        return r;
    }

    static float fdiv(float a, float b) {
        float r = a / b;

        if (loggingEnable) {
            fdivLog.add(a);
            fdivLog.add(b);
            fdivLog.add(r);
        }

        return r;
    }

    static float finv(float a) {
        float r = 1.0f / a;

        if (loggingEnable) {
            finvLog.add(a);
            finvLog.add(r);
        }

        return r;
    }

    static float fsqrt(float a) {
        float r = (float) Math.sqrt((double) a);

        if (loggingEnable) {
            fsqrtLog.add(a);
            fsqrtLog.add(r);
        }

        return r;
    }
}
