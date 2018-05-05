package kdkvsk.hackuo.client.crytpo;

public class LoginCrypto implements Crypto {
    private int table[] = new int[2];
    private int key1;
    private int key2;

    public LoginCrypto(int seed, int key1, int key2) {
        table[0] = (((~seed) ^ 0x00001357) << 16) | ((seed ^ 0xFFFFAAAA) & 0x0000FFFF);
        table[1] = ((seed ^ 0x43210000) >>> 16) | (((~seed) ^ 0xabcdffff) & 0xffff0000);

        this.key1 = key1;
        this.key2 = key2;
    }

    public void encryptClient(byte in[], int inOffset, byte out[], int outOffset, int length) {
        for(int i = 0; i < length; i++) {
            out[i + outOffset] = (byte)(in[i + inOffset] ^ (table[0] & 0xFF));

            int oldKey1 = table[1];

            table[1] = ((((table[1] >>> 1) | (table[0] << 31)) ^ (key1 - 1)) >>> 1 | (table[0] << 31)) ^ key1;
            table[0] = ((table[0] >>> 1) | (oldKey1 << 31)) ^ key2;
        }
    }

    public void decryptServer(byte in[], int inOffset, byte out[], int outOffset, int length) {
        System.arraycopy(in, inOffset, out, outOffset, length);
    }
}
