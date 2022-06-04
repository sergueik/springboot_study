package example.influx.model;

/**
 * description:
 * date: 2022/1/16 23:05
 * author: zhouhong
 */

public class ErrorResponseData extends ResponseData {
    private String exceptionClazz;

    ErrorResponseData(String message) {
        super(false, DEFAULT_ERROR_CODE, message, message, (Object)null);
    }

    public ErrorResponseData(Integer code, String message) {
        super(false, code, message, message, (Object)null);
    }

    ErrorResponseData(Integer code, String message, Object object) {
        super(false, code, message, object);
    }

    ErrorResponseData(Integer code, String message, String localizedMsg, Object object) {
        super(false, code, message, localizedMsg, object);
    }

    @Override
    public boolean equals(final Object o) {
        if (o == this) {
            return true;
        } else if (!(o instanceof ErrorResponseData)) {
            return false;
        } else {
            ErrorResponseData other = (ErrorResponseData)o;
            if (!other.canEqual(this)) {
                return false;
            } else if (!super.equals(o)) {
                return false;
            } else {
                Object this$exceptionClazz = this.getExceptionClazz();
                Object other$exceptionClazz = other.getExceptionClazz();
                if (this$exceptionClazz == null) {
                    return other$exceptionClazz == null;
                } else {
                    return this$exceptionClazz.equals(other$exceptionClazz);
                }
            }
        }
    }

    @Override
    protected boolean canEqual(final Object other) {
        return other instanceof ErrorResponseData;
    }

    @Override
    public int hashCode() {
        int result = super.hashCode();
        Object $exceptionClazz = this.getExceptionClazz();
        result = result * 59 + ($exceptionClazz == null ? 43 : $exceptionClazz.hashCode());
        return result;
    }

    public String getExceptionClazz() {
        return this.exceptionClazz;
    }

    public void setExceptionClazz(final String exceptionClazz) {
        this.exceptionClazz = exceptionClazz;
    }

    @Override
    public String toString() {
        return "ErrorResponseData(exceptionClazz=" + this.getExceptionClazz() + ")";
    }
}
