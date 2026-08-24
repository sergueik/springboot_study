using System;
using System.Collections.Generic;
using System.Text;
using System.Data;
using System.Data.SQLite;
using SQLite.Utils;
using OxyPlot;

namespace SQLite.Utils
{

    public class PlotHelper
    {

        public PlotHelper()
        {
        }

        //  http://stackoverflow.com/questions/7002806/system-data-datasetextensions-in-mono 

        static public IEnumerable<DataPoint> LoadData(string dataSource, string tableName)
        {
            try
            {
            	string sql = String.Format("select * from {0}", tableName) ;
                using (SQLiteConnection conn = new SQLiteConnection(dataSource))
                {
                    using (SQLiteCommand cmd = new SQLiteCommand())
                    {
                        cmd.Connection = conn;
                        conn.Open();
                        SQLiteHelper sh = new SQLiteHelper(cmd);
                        DataTable res = sh.Select(sql);                        
                        IEnumerable<DataPoint> ms = from row in res.AsEnumerable()
                                  orderby row["responsetime"]
                                  select new DataPoint(Convert.ToDouble(row["count"]), Convert.ToDouble(row["responsetime"]));
                        ;

                        conn.Close();

                        return ms;
                    }
                }
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine(ex.ToString());
                return (IEnumerable<DataPoint>)null;
            }

        }
        static public bool InsertPlotData(string dataSource, string tableName, Dictionary<string, object> dic)
        {
            try
            {
                using (SQLiteConnection conn = new SQLiteConnection(dataSource))
                {
                    using (SQLiteCommand cmd = new SQLiteCommand())
                    {
                        cmd.Connection = conn;
                        conn.Open();
                        SQLiteHelper sh = new SQLiteHelper(cmd);
                        sh.Insert(tableName, dic);
                        conn.Close();
                        return true;
                    }
                }
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine(ex.ToString());
                return false;
            }
        }

                public static void createTable(string dataSource, string tableName)
        {
            using (SQLiteConnection conn = new SQLiteConnection(dataSource))
            {
                using (SQLiteCommand cmd = new SQLiteCommand())
                {
                    cmd.Connection = conn;
                    conn.Open();
                    SQLiteHelper sh = new SQLiteHelper(cmd);
                    sh.DropTable(tableName);

                    SQLiteTable tb = new SQLiteTable(tableName);
                    tb.Columns.Add(new SQLiteColumn("id", true)); // auto increment 
                    tb.Columns.Add(new SQLiteColumn("count"));
                    tb.Columns.Add(new SQLiteColumn("responsetime", ColType.Decimal));
                    sh.CreateTable(tb);
                    conn.Close();
                }
            }
        }

        public static bool TestConnection(string dataSource)
        {
            Console.WriteLine(String.Format("Testing database source connection`n{0}...", dataSource));
            try
            {
                using (SQLiteConnection conn = new SQLiteConnection(dataSource))
                {
                    conn.Open();
                    conn.Close();
                }
                return true;
            }

            catch (Exception ex)
            {
                Console.Error.WriteLine(ex.ToString());
                return false;
            }
        }

    }
}