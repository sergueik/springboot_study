@RestController
@RequestMapping("/api")
public class ExampleController {

    @GetMapping("/profile")
    public String profile() {
        return "authenticated user";
    }


    @PreAuthorize("hasAuthority('APPROLE_Admin')")
    @DeleteMapping("/administration")
    public String administrativeAction() {
        return "admin operation";
    }
}
