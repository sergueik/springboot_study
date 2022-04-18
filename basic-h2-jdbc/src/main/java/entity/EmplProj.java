package entity;

/**
 * Created by vserdiuk on 2/4/17.
 */
public class EmplProj {

    private Long employeeId;
    private Long projectId;

    public EmplProj() {

    }

    public Long getEmployeeId() {
        return employeeId;
    }

    public void setEmployeeId(Long employeeId) {
        this.employeeId = employeeId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        EmplProj emplProj = (EmplProj) o;

        if (employeeId != null ? !employeeId.equals(emplProj.employeeId) : emplProj.employeeId != null) return false;
        return projectId != null ? projectId.equals(emplProj.projectId) : emplProj.projectId == null;
    }

    @Override
    public int hashCode() {
        int result = employeeId != null ? employeeId.hashCode() : 0;
        result = 31 * result + (projectId != null ? projectId.hashCode() : 0);
        return result;
    }

    @Override
    public String toString() {
        return "EmplProj{" +
                "employeeId=" + employeeId +
                ", projectId=" + projectId +
                '}';
    }
}
