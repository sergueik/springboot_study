using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using EfcoreTest.Data.Context;
using EfcoreTest.Data.Entities;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;

namespace EfcoreTest.Api.Controllers
{
    /// <summary>
    /// An API controller providing CRUD operations for todo items.
    /// </summary>
    [ApiController]
    [Route("[controller]")]
    public class TodoController : ControllerBase
    {
        // configure database context and logging tools
        private readonly TodoContext _dbContext;
        private readonly ILogger<TodoController> _logger;
        public TodoController(ILogger<TodoController> logger, TodoContext context) {
            _logger = logger;
            _dbContext = context;
        }

        /// <summary>
        /// Retrieve a list of all todo items.
        /// </summary>
        /// <returns>a list of todo items</returns>
        [HttpGet]
        public IEnumerable<TodoItem> GetAllTodos()
            => _dbContext.Todos.ToList();

        /// <summary>
        /// Retrieve a single todo item by id.
        /// </summary>
        /// <param name="id">The id to be looked up.</param>
        /// <returns>a todo item (or null if it does not exist)</returns>
        [HttpGet("{id}")]
        public async Task<TodoItem> GetTodoById(int id)
            => await _dbContext.Todos.FindAsync(id);

        /// <summary>
        /// Add a new todo item to the list.
        /// </summary>
        /// <param name="item">The todo item to be added.</param>
        /// <returns>a copy of the actual added todo item</returns>
        [HttpPost]
        public async Task<TodoItem> CreateTodo([FromBody] TodoItem item)
        {
            // create the todo item
            var newItemRef = await _dbContext.Todos.AddAsync(item);

            // apply the changes to database
            await _dbContext.SaveChangesAsync();

            // return the newly created item
            // (with the id assigned to it)
            return newItemRef.Entity;
        }

        /// <summary>
        /// Update an existing todo item.
        /// </summary>
        /// <param name="updItem">The todo item with all the data to be updated.</param>
        [HttpPut]
        public async Task UpdateTodo([FromBody] TodoItem updItem)
        {
            // update the given todo item
            _dbContext.Update(updItem);

            // apply the changes to database
            await _dbContext.SaveChangesAsync();
        }

        /// <summary>
        /// Remove the todo item with the given id from the list.
        /// </summary>
        /// <param name="id">The id of the item to be removed.</param>
        /// <returns>The todo item that was just deleted.</returns>
        [HttpDelete("{id}")]
        public async Task<TodoItem> DeleteTodo(int id)
        {
            // find the todo item to be removed
            var item = await _dbContext.FindAsync<TodoItem>(id);

            // make sure the item exists, otherwise abort
            if (item == null) { throw new KeyNotFoundException(
                "The item you are attempting to delete does not seem to exist!"); }

            // remove the item from the database
            var delItem = _dbContext.Remove(item).Entity;
            await _dbContext.SaveChangesAsync();

            return delItem;
        }
    }
}
