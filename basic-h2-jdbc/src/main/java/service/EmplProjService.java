package service;

import bl.Util;
import dao.EmplProjDAO;
import entity.EmplProj;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;

public class EmplProjService extends Util implements EmplProjDAO {

    private Connection connection = getConnection();

    @Override
    public void add(EmplProj emplProj) throws SQLException {
        PreparedStatement preparedStatement = null;

        String sql = "INSERT INTO EMPL_PROJ (EMPLOYEE_ID, PROJECT_ID) VALUES(?, ?)";

        try {
            preparedStatement = connection.prepareStatement(sql);

            preparedStatement.setLong(1, emplProj.getEmployeeId());
            preparedStatement.setLong(2, emplProj.getProjectId());

            preparedStatement.executeUpdate();
        } catch (SQLException e) {
            e.printStackTrace();
        } finally {
            if (preparedStatement != null) {
                preparedStatement.close();
            }
            if (connection != null) {
                connection.close();
            }
        }
    }

    @Override
    public List<EmplProj> getAll() throws SQLException {
        List<EmplProj> emplProjList = new ArrayList<>();

        String sql = "SELECT EMPLOYEE_ID, PROJECT_ID FROM EMPL_PROJ";

        Statement statement = null;
        try {
            statement = connection.createStatement();

            ResultSet resultSet = statement.executeQuery(sql);

            while (resultSet.next()) {
                EmplProj emplProj = new EmplProj();
                emplProj.setEmployeeId(resultSet.getLong("EMPLOYEE_ID"));
                emplProj.setProjectId(resultSet.getLong("PROJECT_ID"));

                emplProjList.add(emplProj);
            }
        } catch (SQLException e) {
            e.printStackTrace();
        } finally {
            if (statement != null) {
                statement.close();
            }
            if (connection != null) {
                connection.close();
            }
        }
        return emplProjList;
    }

    @Override
    public EmplProj getByEmployeeIdAndProjectId(Long employeeId, Long projectId) throws SQLException {
        PreparedStatement preparedStatement = null;

        String sql = "SELECT EMPLOYEE_ID, PROJECT_ID FROM EMPL_PROJ WHERE EMPLOYEE_ID=? AND PROJECT_ID=?";
        EmplProj emplProj = new EmplProj();
        try {
            preparedStatement = connection.prepareStatement(sql);
            preparedStatement.setLong(1, employeeId);
            preparedStatement.setLong(2, projectId);

            ResultSet resultSet = preparedStatement.executeQuery();

            emplProj.setEmployeeId(resultSet.getLong("EMPLOYEE_ID"));
            emplProj.setProjectId(resultSet.getLong("PROJECT_ID"));

            preparedStatement.executeUpdate();
        } catch (SQLException e) {
            e.printStackTrace();
        } finally {
            if (preparedStatement != null) {
                preparedStatement.close();
            }
            if (connection != null) {
                connection.close();
            }
        }
        return emplProj;
    }

    @Override
    public void update(EmplProj emplProj) throws SQLException {
        PreparedStatement preparedStatement = null;

        String sql = "UPDATE EMPL_PROJ SET EMPLOYEE_ID=?, PROJECT_ID=?";

        try {
            preparedStatement = connection.prepareStatement(sql);

            preparedStatement.setLong(1, emplProj.getEmployeeId());
            preparedStatement.setLong(2, emplProj.getProjectId());

            preparedStatement.executeUpdate();
        } catch (SQLException e) {
            e.printStackTrace();
        } finally {
            if (preparedStatement != null) {
                preparedStatement.close();
            }
            if (connection != null) {
                connection.close();
            }
        }
    }

    @Override
    public void remove(EmplProj emplProj) throws SQLException {
        PreparedStatement preparedStatement = null;

        String sql = "DELETE FROM EMPL_PROJ WHERE EMPLOYEE_ID=? AND PROJECT_ID=?";

        try {
            preparedStatement = connection.prepareStatement(sql);

            preparedStatement.setLong(1, emplProj.getEmployeeId());
            preparedStatement.setLong(2, emplProj.getProjectId());

            preparedStatement.executeUpdate();
        } catch (SQLException e) {
            e.printStackTrace();
        } finally {
            if (preparedStatement != null) {
                preparedStatement.close();
            }
            if (connection != null) {
                connection.close();
            }
        }
    }
}
