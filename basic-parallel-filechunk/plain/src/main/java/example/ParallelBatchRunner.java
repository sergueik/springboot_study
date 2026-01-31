import java.io.*;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.*;

public class ParallelBatchRunner {

    record FileChunk(long start, long size, int index) {}

    public static void main(String[] args) throws Exception {
        Path file = Paths.get("test-batch.dat");
        generateTestFile(file, 20, 64); // 20 records, 64 bytes each
        new ParallelBatchRunner().run(file);
    }

    // ---------------- BOOTSTRAP ----------------

    public void run(Path file) throws Exception {

        System.out.println("Runner hash=" + System.identityHashCode(this));

        long chunkSize = pretendJRecordDetermineChunkSize();
        System.out.println("Pretend JRecord chunk size = " + chunkSize);

        List<FileChunk> chunks = chunkFile(file, chunkSize);

        ExecutorService executor = Executors.newFixedThreadPool(4);
        List<Future<?>> futures = new ArrayList<>();

        for (FileChunk chunk : chunks) {
            ChunkWorker worker = new ChunkWorker(file, chunk);
            System.out.println("Worker hash=" + System.identityHashCode(worker));

            futures.add(executor.submit(worker));
        }

        // ---- CRITICAL: WAIT FOR ALL JOBS ----
        for (Future<?> f : futures) {
            f.get(); // blocks until each finishes
        }

        executor.shutdown();
        System.out.println("ALL JOBS FINISHED");
    }

    // ---------------- PRETEND JRECORD ----------------

    private long pretendJRecordDetermineChunkSize() {
        PretendJRecord parser = new PretendJRecord();
        System.out.println("PretendJRecord hash=" + System.identityHashCode(parser));
        return 128; // pretend JRecord decided this
    }

    // ---------------- CHUNKING ----------------

    private List<FileChunk> chunkFile(Path file, long chunkSize) throws IOException {
        long fileSize = Files.size(file);

        List<FileChunk> chunks = new ArrayList<>();
        int index = 0;

        for (long pos = 0; pos < fileSize; pos += chunkSize) {
            long size = Math.min(chunkSize, fileSize - pos);
            chunks.add(new FileChunk(pos, size, index++));
        }

        return chunks;
    }

    // ---------------- WORKER ----------------

    static class ChunkWorker implements Runnable {

        private final Path file;
        private final FileChunk chunk;

        ChunkWorker(Path file, FileChunk chunk) {
            this.file = file;
            this.chunk = chunk;
        }

        @Override
        public void run() {
            String thread = Thread.currentThread().getName();

            PretendJRecord parser = new PretendJRecord();

            System.out.printf(
                "Thread=%s WorkerHash=%d ParserHash=%d Chunk=%d%n",
                thread,
                System.identityHashCode(this),
                System.identityHashCode(parser),
                chunk.index()
            );

            try (FileChannel fc = FileChannel.open(file, StandardOpenOption.READ)) {

                System.out.printf("Thread=%s FileChannelHash=%d%n",
                        thread, System.identityHashCode(fc));

                fc.position(chunk.start());

                ByteBuffer buffer = ByteBuffer.allocate((int) chunk.size());
                System.out.printf("Thread=%s BufferHash=%d%n",
                        thread, System.identityHashCode(buffer));

                fc.read(buffer);
                buffer.flip();

                parser.parse(buffer, chunk, thread);

            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    // ---------------- PRETEND JRECORD PARSER ----------------

    static class PretendJRecord {

        void parse(ByteBuffer buffer, FileChunk chunk, String thread) {
            long count = 0;
            byte first = buffer.get(0);

            while (buffer.hasRemaining()) {
                buffer.get();
                count++;
            }

            System.out.printf(
                "Thread=%s PretendJRecordHash=%d Chunk=%d FirstByte=%c Bytes=%d%n",
                thread,
                System.identityHashCode(this),
                chunk.index(),
                (char) first,
                count
            );
        }
    }

    // ---------------- TEST FILE GENERATOR ----------------

    static void generateTestFile(Path file, int records, int recordSize) throws IOException {

        try (OutputStream out = Files.newOutputStream(file)) {
            for (int i = 1; i <= records; i++) {
                byte b = (byte) ('0' + (i % 10));
                for (int j = 0; j < recordSize; j++) {
                    out.write(b);
                }
            }
        }

        System.out.println("Generated file: " + file.toAbsolutePath());
    }
}

