package kdkvsk.hackuo.lib.crytpo;

import org.bouncycastle.crypto.engines.TwofishEngine;
import org.bouncycastle.crypto.params.KeyParameter;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class GameCrypto implements Crypto {
    private TwofishEngine engine = new TwofishEngine();

    private byte[] twofishCypherTable;
    private int twofishPos = 0;

    private byte[] md5XorTable;
    private int md5XorPos = 0;

    public GameCrypto(int seed) {
        byte[] key = makeKey(seed);
        KeyParameter params = new KeyParameter(key);
        engine.init(true, params);

        twofishCypherTable = new byte[256];
        for (int i = 0; i < 256; i++) {
            twofishCypherTable[i] = (byte)i;
        }

        refreshTwofishCypherTable();

        try {
            MessageDigest md5 = MessageDigest.getInstance("MD5");
            md5XorTable = md5.digest(twofishCypherTable);
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException("can't create MD5 digest", e);
        }
    }

    private void refreshTwofishCypherTable() {
        byte[] block = new byte[16];
        int blockSize = 0;

        for (int i = 0; i < 256; ) {
            blockSize = engine.processBlock(twofishCypherTable, i, block, 0);
            System.arraycopy(block, 0, twofishCypherTable, i, blockSize);

            i = i + blockSize;
        }

        twofishPos = 0;
    }

    private byte[] makeKey(int seed) {
        byte[] key = new byte[16];

        key[0] = key[4] = key[8] = key[12] = (byte)((seed >>> 24) & 0xff);
        key[1] = key[5] = key[9] = key[13] = (byte)((seed >>> 16) & 0xff);
        key[2] = key[6] = key[10] = key[14] = (byte)((seed >>> 8) & 0xff);
        key[3] = key[7] = key[11] = key[15] = (byte)(seed & 0xff);

        return key;
    }

    public void decryptServer(byte in[], int inOffset, byte out[], int outOffset, int length) {
        for (int i = 0; i < length; i++) {
            out[i + outOffset] = (byte)(in[i + inOffset] ^ md5XorTable[md5XorPos++]);
            md5XorPos &= 0x0F;
        }
    }

    public void encryptClient(byte in[], int inOffset, byte out[], int outOffset, int length) {
        for (int i = 0; i < length; i++) {
            if (twofishPos >= 256) {
                refreshTwofishCypherTable();
            }

            out[i + outOffset] = (byte)(in[i + inOffset] ^ twofishCypherTable[twofishPos++]);
        }
    }
}
