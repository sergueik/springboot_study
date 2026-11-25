using OfficeIMO.Word;
using OfficeIMO.Word.Html;

namespace PdfDocGenerator.CLI
{
    public class Program
    {
        public  static void Main(string[] args)
        {
		using (var report = WordDocument.Create("Sales.docx")) {
    // report.AddHeading("Quarterly sales", level: 1);
    // report.AddParagraph("Revenue by region for the third quarter.");
  //  report.AddTable(sales);
    // report.AddChart(OfficeChartType.Column, sales, nameof(Sale.Region), nameof(Sale.Revenue));
    report.Save();
    // report.SaveAsPdf("Sales.pdf");
    //
}
   //         var doc = WordDocument.FromHtml("<h1>Hi</h1><p>Generated</p>");
        }
    }
}
