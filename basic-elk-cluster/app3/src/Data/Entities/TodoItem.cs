using System;
using System.ComponentModel.DataAnnotations;

namespace EfcoreTest.Data.Entities
{
    public class TodoItem
    {
        [Key]
        public int Id { get; set; }

        [Required]
        public string Text { get; set; }

        [Required]
        public DateTime? Due { get; set; } = null;
    }
}
