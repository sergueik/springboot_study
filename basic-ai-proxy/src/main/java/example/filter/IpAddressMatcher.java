package example.filter;

import java.net.InetAddress;
import java.net.UnknownHostException;

public class IpAddressMatcher {
    private final String ipAddress;
    private final int prefixLength;

    public IpAddressMatcher(String ipAddress) {
        if (ipAddress.indexOf('/') > 0) {
            String[] parts = ipAddress.split("/", 2);

            this.ipAddress = parts[0];
            this.prefixLength = Integer.parseInt(parts[1]);
        } else {
            this.ipAddress = ipAddress;
            this.prefixLength = -1;
        }
    }

    public boolean matches(String remoteAddress) {
        try {

            InetAddress remote = InetAddress.getByName(remoteAddress);
            InetAddress requiredAddress = InetAddress.getByName(this.ipAddress);

            if (!remote.getClass().equals(requiredAddress.getClass())) {
                return false;
            }

            if (prefixLength < 0) {
                return remote.equals(requiredAddress);
            }

            byte[] remoteBytes = remote.getAddress();
            byte[] requiredBytes = requiredAddress.getAddress();

            int fullBytes = prefixLength / 8;
            int remainingBits = prefixLength % 8;

            for (int i = 0; i < fullBytes; i++) {
                if (remoteBytes[i] != requiredBytes[i]) {
                    return false;
                }
            }

            if (remainingBits > 0) {
                int mask = 0xFF << (8 - remainingBits);
                if ((remoteBytes[fullBytes] & mask) != (requiredBytes[fullBytes] & mask)) {
                    return false;
                }
            }
            return true;
        } catch (UnknownHostException e) {
            return false;
        }
    }
}