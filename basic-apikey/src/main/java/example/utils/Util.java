package example.utils;

import org.apache.commons.codec.binary.Hex;

import java.nio.ByteBuffer;
import java.util.UUID;

public final class Util {

	private Util() {
		// Noop
	}

	public static UUID fromHex(String uuid) throws Exception {
		byte[] data = Hex.decodeHex(uuid.toCharArray());
		return new UUID(ByteBuffer.wrap(data, 0, 8).getLong(),
				ByteBuffer.wrap(data, 8, 8).getLong());
	}

	public static String toHex(UUID uuid) {
		ByteBuffer bytes = ByteBuffer.wrap(new byte[16]);
		bytes.putLong(uuid.getMostSignificantBits());
		bytes.putLong(uuid.getLeastSignificantBits());
		return new String(Hex.encodeHex(bytes.array()));
	}
}