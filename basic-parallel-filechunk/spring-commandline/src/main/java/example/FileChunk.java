package example;

public class FileChunk {

    private final int id;
    private final long startOffset;
    private final long endOffset;

    public FileChunk(int id, long startOffset, long endOffset) {
        this.id = id;
        this.startOffset = startOffset;
        this.endOffset = endOffset;
    }

    public int getId() {
        return id;
    }

    public long getStartOffset() {
        return startOffset;
    }

    public long getEndOffset() {
        return endOffset;
    }

    public long size() {
        return endOffset - startOffset;
    }

    @Override
    public String toString() {
        return "FileChunk{id=" + id +
               ", start=" + startOffset +
               ", end=" + endOffset + '}';
    }
}
