using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using AspNetCoreDockerExample.Models;

namespace AspNetCoreDockerExample.Controllers
{
    [ApiController]
    [Route("/api/user")]
    public class UserController : ControllerBase
    {
        List<UserModel> Users = new List<UserModel>() {};        

        [HttpGet("{Username}")]
        public IActionResult GetUser(string Username)
        {
            var res = Users.Find(x => x.Username == Username);
            if (res != null)
            {   
                return Ok(new { Statu = "Succes" });
            }
            else
            {
                return Unauthorized(new { Statu = "Fail", Error = "Invalid Username" });
            }
        }

        [HttpPost]
        public IActionResult PostUser(QueryModel Model)
        {
            var res = Users.Find(x => x.Username == Model.Username);
            
            if (res != null)
            {
                return BadRequest(new {Statu = "Fail", Error = "Username Already Exıst: " + Model.Username });
            }
            else if (Model.Username.Length <= 5)
            {
                return BadRequest(new { Statu = "Fail", Error = "This Username is too short" });
            }
            else
            {
                Users.Add(new UserModel{Username = Model.Username});
                return Ok(new { Statu = "Succes" });
            }
        }
    }
}
