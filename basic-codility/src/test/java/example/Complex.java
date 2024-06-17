package example;

public class Complex {

    private final String data;
    private final String text;

    Complex(Builder builder) {
        data = builder.data;
        text = builder.text;
    }

    public String getData() {
        return data;
    }

    public String getText() {
        return text;
    }

    public static class Builder {
        private String data;
        private String text;

        public Builder data(String value) {
            data = value;
            return this;
        }

        public Builder text(String value) {
            text = value;
            return this;
        }

        public Complex build() {
            return new Complex (this);
        }
    }
}

/*
Complex complex = new Complex.Builder()
  .data("Java Builder Pattern")
  .build();
*/

