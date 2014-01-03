package cpuex4;

import java.io.*;

class FPU {
    private boolean dumpEnable = false;
    private BinWriter faddOut;
    private BinWriter fsubOut;
    private BinWriter fmulOut;
    private BinWriter fdivOut;
    private BinWriter finvOut;
    private BinWriter fsqrtOut;
    private long faddCount = 0;
    private long fsubCount = 0;
    private long fmulCount = 0;
    private long fdivCount = 0;
    private long finvCount = 0;
    private long fsqrtCount = 0;
    private long splitCount = 0;
    private long SPLIT_SIZE = 1000000;
    private long SPLIT_LIMIT = 200;

    FPU(boolean dump) {
        dumpEnable = dump;
        if (dumpEnable) {
            Runtime.getRuntime().addShutdownHook(new Thread(new Runnable() {
                public void run() {
                    try {
                        if (faddOut != null) faddOut.close();
                        if (fsubOut != null) fsubOut.close();
                        if (fmulOut != null) fmulOut.close();
                        if (fdivOut != null) fdivOut.close();
                        if (finvOut != null) finvOut.close();
                        if (fsqrtOut != null) fsqrtOut.close();
                    } catch (IOException e) {
                    }
                }
            }));
        }
    }

    // TODO: These implementation should be functionally equivalent to FPU's 

    float fadd(float a, float b) throws IOException {
        float r = a + b;

        if (dumpEnable && splitCount <= SPLIT_LIMIT) {
            if (faddCount % SPLIT_SIZE == 0 ) {
                splitCount++;
                if (faddOut != null) {
                    faddOut.close();
                }
                if (splitCount > SPLIT_LIMIT) {
                    return r;
                }
                String filename = String.format("fadd.%d", faddCount / SPLIT_SIZE);
                faddOut = new BinWriter(new BufferedOutputStream(new FileOutputStream(filename)));
            }
            faddCount++;
            faddOut.writeFloat(a);
            faddOut.writeFloat(b);
            faddOut.writeFloat(r);
        }

        return r;
    }

    float fsub(float a, float b) throws IOException {
        float r = a - b;

        if (dumpEnable && splitCount <= SPLIT_LIMIT) {
            if (fsubCount % SPLIT_SIZE == 0 ) {
                splitCount++;
                if (fsubOut != null) {
                    fsubOut.close();
                }
                if (splitCount > SPLIT_LIMIT) {
                    return r;
                }
                String filename = String.format("fsub.%d", fsubCount / SPLIT_SIZE);
                fsubOut = new BinWriter(new BufferedOutputStream(new FileOutputStream(filename)));
            }
            fsubCount++;
            fsubOut.writeFloat(a);
            fsubOut.writeFloat(b);
            fsubOut.writeFloat(r);
        }

        return r;
    }

    float fmul(float a, float b) throws IOException {
        float r = a * b;

        if (dumpEnable && splitCount <= SPLIT_LIMIT) {
            if (fmulCount % SPLIT_SIZE == 0 ) {
                splitCount++;
                if (fmulOut != null) {
                    fmulOut.close();
                }
                if (splitCount > SPLIT_LIMIT) {
                    return r;
                }
                String filename = String.format("fmul.%d", fmulCount / SPLIT_SIZE);
                fmulOut = new BinWriter(new BufferedOutputStream(new FileOutputStream(filename)));
            }
            fmulCount++;
            fmulOut.writeFloat(a);
            fmulOut.writeFloat(b);
            fmulOut.writeFloat(r);
        }

        return r;
    }

    float fdiv(float a, float b) throws IOException {
        float r = a / b;

        if (dumpEnable && splitCount <= SPLIT_LIMIT) {
            if (fdivCount % SPLIT_SIZE == 0 ) {
                splitCount++;
                if (fdivOut != null) {
                    fdivOut.close();
                }
                if (splitCount > SPLIT_LIMIT) {
                    return r;
                }
                String filename = String.format("fdiv.%d", fdivCount / SPLIT_SIZE);
                fdivOut = new BinWriter(new BufferedOutputStream(new FileOutputStream(filename)));
            }
            fdivCount++;
            fdivOut.writeFloat(a);
            fdivOut.writeFloat(b);
            fdivOut.writeFloat(r);
        }

        return r;
    }

    float finv(float a) throws IOException {
        float r = 1.0f / a;

        if (dumpEnable && splitCount <= SPLIT_LIMIT) {
            if (finvCount % SPLIT_SIZE == 0 ) {
                splitCount++;
                if (finvOut != null) {
                    finvOut.close();
                }
                if (splitCount > SPLIT_LIMIT) {
                    return r;
                }
                String filename = String.format("finv.%d", finvCount / SPLIT_SIZE);
                finvOut = new BinWriter(new BufferedOutputStream(new FileOutputStream(filename)));
            }
            finvCount++;
            finvOut.writeFloat(a);
            finvOut.writeFloat(r);
        }

        return r;
    }

    float fsqrt(float a) throws IOException {
        float r = (float) Math.sqrt((double) a);

        if (dumpEnable && splitCount <= SPLIT_LIMIT) {
            if (fsqrtCount % SPLIT_SIZE == 0 ) {
                splitCount++;
                if (fsqrtOut != null) {
                    fsqrtOut.close();
                }
                if (splitCount > SPLIT_LIMIT) {
                    return r;
                }
                String filename = String.format("fsqrt.%d", fsqrtCount / SPLIT_SIZE);
                fsqrtOut = new BinWriter(new BufferedOutputStream(new FileOutputStream(filename)));
            }
            fsqrtCount++;
            fsqrtOut.writeFloat(a);
            fsqrtOut.writeFloat(r);
        }

        return r;
    }
}
