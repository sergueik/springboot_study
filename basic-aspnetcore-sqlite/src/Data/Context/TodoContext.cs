using Microsoft.EntityFrameworkCore;
using EfcoreTest.Data.Entities;
using System.Reflection;

namespace EfcoreTest.Data.Context
{
    public class TodoContext : DbContext
    {
        protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
        {
            // configure the database connection
            optionsBuilder.UseSqlite("Filename=TodoDatabase.db", options => {
                options.MigrationsAssembly(Assembly.GetExecutingAssembly().FullName);
            });

            // apply the db connection configuration
            base.OnConfiguring(optionsBuilder);
        }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            // define the data schema to be created on-the-fly
            modelBuilder.Entity<TodoItem>().ToTable("Todos", "test");
            modelBuilder.Entity<TodoItem>(entity =>
            {
                entity.HasKey(e => e.Id);
                entity.Property(e => e.Text).IsRequired();
                entity.Property(e => e.Due).HasDefaultValue(null);
            });

            // create the schema on startup if it does not exist yet
            base.OnModelCreating(modelBuilder);
        }

        // the list of tables to be mapped
        public DbSet<TodoItem> Todos { get; set; }
    }
}
