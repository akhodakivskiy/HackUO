package kdkvsk.hackuo.lib.crytpo;

public interface Crypto {
     void encryptClient(byte in[], int inOffset, byte out[], int outOffset, int length);
     void decryptServer(byte in[], int inOffset, byte out[], int outOffset, int length);
}
