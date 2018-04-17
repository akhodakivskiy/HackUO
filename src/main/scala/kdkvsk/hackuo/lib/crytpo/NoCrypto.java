package kdkvsk.hackuo.lib.crytpo;

public class NoCrypto implements Crypto {
    public void encryptClient(byte in[], int inOffset, byte out[], int outOffset, int length) {
        System.arraycopy(in, inOffset, out, outOffset, length);
    }

    public void decryptServer(byte in[], int inOffset, byte out[], int outOffset, int length) {
        System.arraycopy(in, inOffset, out, outOffset, length);
    }
}
