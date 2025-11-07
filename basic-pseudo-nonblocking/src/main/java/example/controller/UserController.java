package example.controller;

import org.springframework.http.HttpEntity;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.context.request.async.DeferredResult;

import java.util.Map;
import java.util.concurrent.Callable;
import java.lang.Runnable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ForkJoinPool;

import example.model.User;
import example.service.UserService;

@RestController
@RequestMapping("/users")
public class UserController {

  private final Map<Long, User> users = new ConcurrentHashMap<>();

  @GetMapping(value = "/{id}", produces = { MediaType.APPLICATION_JSON_VALUE })
  public DeferredResult<HttpEntity<User>> getUser(@PathVariable("id") long id) {
    DeferredResult<HttpEntity<User>> result = new DeferredResult<>();

  Runnable producer = () -> {
    try {
      User user = users.get(id);
      if (user == null) {
        result.setResult(ResponseEntity.status(404).body(null));
      } else {
        result.setResult(ResponseEntity.ok(user));
      }
    } catch (Exception ex) {
      result.setResult(ResponseEntity.status(500)
          .body(new User(-1L, "Error: " + ex.getMessage(), null)));
    }
  };

  ForkJoinPool.commonPool().submit(producer);
  return result;
}



  @PostMapping(value = "", consumes = { MediaType.APPLICATION_JSON_VALUE },
                        produces = { MediaType.APPLICATION_JSON_VALUE })
  public Callable<HttpEntity<User>> addUser(@RequestBody User user) {
    Callable<HttpEntity<User>> producer = () -> {
      users.put(user.getId(), user);
      return ResponseEntity.ok(user);
    };
    return producer;
  }
}

