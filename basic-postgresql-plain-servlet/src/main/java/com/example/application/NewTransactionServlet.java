package com.example.application;

import java.io.*;
import java.sql.*;
import java.util.Optional;

import jakarta.servlet.http.*;

public class NewTransactionServlet extends HttpServlet {
    private final String URL = "jdbc:postgresql://localhost:5432/application_db";
    private final String USER = "postgres";
    private final String PASSWORD = "1";

    @Override
    public void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        response.setContentType("text/html");

        //Извлекаем параметры из URL
        Integer playerId = Optional.ofNullable(request.getParameter("playerId"))
                .map(Integer::parseInt)
                .orElseThrow(() -> new IllegalStateException("playerId is null"));

        Integer clanId = Optional.ofNullable(getClanId(playerId))
                .orElseThrow(() -> new IllegalStateException("player has no clan"));

        Integer money = Optional.ofNullable(request.getParameter("moneyAmount"))
                .map(Integer::parseInt)
                .orElseThrow(() -> new IllegalStateException("moneyAmount is empty"));
        String action = request.getParameter("action");

        long transId = newTrans(playerId, clanId, action, money);
        updateClansGold(clanId, money);

        String message = "Транзакция выполнена успешно, номер транзакции: " + transId;
        PrintWriter out = response.getWriter();
        out.println("<html><body>");
        out.println("<h1>" + message + "</h1>");
        out.println("</body></html>");
    }



    //метод возвращает id клана, на вход подается id игрока
    public Integer getClanId(Integer playerId) {
        String SQL = "select clan_id from player"
                + "where player_id = ?";
        Integer result = null;
        try (
            Connection connection = connect();
            PreparedStatement pstmt = connection.prepareStatement(SQL)) {

                pstmt.setInt(1, playerId);
                ResultSet rs = pstmt.executeQuery();
                if (rs.next())
                    result = Integer.parseInt(rs.getString("clan_id"));
        } catch (SQLException e) {
            e.printStackTrace();
        }
        return result;
    }



    //подключение к БД
    public Connection connect() throws SQLException {
        try {
            Class.forName("org.postgresql.Driver");
        }
        catch (ClassNotFoundException eConnection) {
            eConnection.printStackTrace();
        }
        return DriverManager.getConnection(URL, USER, PASSWORD);
    }



    //новая транзакция, метод возвращает id транзакции
    public long newTrans(Integer playerId, Integer clanId, String action, Integer money) {
        String SQL = "INSERT INTO transactions (player_id, clan_id, action, money) "
                + "VALUES(?,?,?,?)";
        long id = 0;
        try (Connection conn = connect();
             PreparedStatement pstmt = conn.prepareStatement(SQL,
                     Statement.RETURN_GENERATED_KEYS)) {
            pstmt.setInt(1, playerId);
            pstmt.setInt(2,clanId);
            pstmt.setString(3, action);
            pstmt.setInt(4, money);

            int affectedRows = pstmt.executeUpdate();
            // check the affected rows
            if (affectedRows > 0) {
                // get the ID back
                try (ResultSet rs = pstmt.getGeneratedKeys()) {
                    if (rs.next()) {
                        id = rs.getLong(1);
                    }
                } catch (SQLException ex) {
                    System.out.println(ex.getMessage());
                }
            }
        } catch (SQLException ex) {
            ex.printStackTrace();
        }
        return id;
    }



    //метод обновляет казну клана
    public void updateClansGold(Integer clanId, Integer money) {
        String SQL = "UPDATE clan "
                + "SET gold = gold + ?"
                + "WHERE clan_id = ?";

        try (Connection conn = connect();
             PreparedStatement pstmt = conn.prepareStatement(SQL)) {

            pstmt.setInt(1, money);
            pstmt.setInt(2, clanId);
            pstmt.executeUpdate();

        } catch (SQLException ex) {
            ex.printStackTrace();
        }
    }
}