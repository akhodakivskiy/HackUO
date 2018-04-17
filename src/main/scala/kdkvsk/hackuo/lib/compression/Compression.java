package kdkvsk.hackuo.lib.compression;

public abstract class Compression {
    public abstract Result decompress(byte in[], int offsetIn, int sizeIn, byte out[], int offsetOut);

    public static class Result {
        Result(int inSize, int outSize, boolean seenBoundary) {
            this.inSize = inSize;
            this.outSize = outSize;
            this.seenBoundary = seenBoundary;
        }

        public int inSize;
        public int outSize;
        public boolean seenBoundary;
    }
}
