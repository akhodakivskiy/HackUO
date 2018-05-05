package kdkvsk.hackuo.client.compression;

public class NoCompression extends Compression {
    public Compression.Result decompress(byte in[], int offsetIn, int sizeIn, byte out[], int offsetOut) {
        System.arraycopy(in, offsetIn, out, offsetOut, sizeIn);
        return new Result(sizeIn, sizeIn, true);
    }
}
