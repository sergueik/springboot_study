package example;

import java.io.FileOutputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;

public class Generator {

    // EBCDIC charset for Mainframe DISPLAY
    private static final Charset EBCDIC = Charset.forName("CP037");

    public static void main(String[] args) throws IOException {
        String customerId = "ABC123";
        BigDecimal balance = new BigDecimal("1050.75");

        int totalBytes = 6  // CUSTOMER-ID DISPLAY
                + 10 // NAME DISPLAY (example)
                + 5  // ACCOUNT COMP-3 S9(9)
                + 3; // BALANCE COMP-3 S9(5)V99
  
        // allocate buffer: 6 bytes for CUSTOMER-ID + 3 bytes for COMP-3 (S9(5)V99)
        ByteBuffer buffer = ByteBuffer.allocate(totalBytes);
        
        // 1️⃣ CUSTOMER-ID -> EBCDIC
        buffer.put(customerId.getBytes(EBCDIC));

        // 2️⃣ BALANCE -> COMP-3 packed decimal
        buffer.put(packComp3(balance, 5, 2)); // 5 digits before decimal, 2 after

        // 3️⃣ Write to file
        try (FileOutputStream fos = new FileOutputStream("dummy_ebcdic_comp3.bin")) {
            fos.write(buffer.array());
        }

        System.out.println("✅ Dummy EBCDIC + COMP-3 row written to dummy_ebcdic_comp3.bin");
    }

    /**
     * Pack a decimal number into COMP-3 (packed decimal).
     * @param value BigDecimal value
     * @param intDigits integer digits (without decimal)
     * @param decDigits fractional digits
     * @return byte[] packed decimal
     */
    private static byte[] packComp3(BigDecimal value, int intDigits, int decDigits) {
        // scale value according to decimal digits
        long scaled = value.movePointRight(decDigits).longValueExact();

        // total nibbles = digits + 1 for sign
        int totalNibbles = intDigits + decDigits + 1;
        int len = (totalNibbles + 1) / 2; // total bytes
        byte[] result = new byte[len];

        long tmp = scaled;
        for (int i = len - 1; i >= 0; i--) {
            int lowNibble;
            if (i == len - 1) {
                // last byte: low nibble = sign (0xC = positive, 0xD = negative)
                lowNibble = 0xC;
            } else {
                lowNibble = (int)(tmp % 10);
                tmp /= 10;
            }
            int highNibble = (int)(tmp % 10);
            tmp /= 10;
            result[i] = (byte)((highNibble << 4) | lowNibble);
        }

        return result;
    }
}
