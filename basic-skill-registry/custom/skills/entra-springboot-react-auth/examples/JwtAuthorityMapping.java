@Bean
JwtAuthenticationConverter jwtAuthenticationConverter() {

    JwtGrantedAuthoritiesConverter converter =
        new JwtGrantedAuthoritiesConverter();

    converter.setAuthoritiesClaimName("roles");
    converter.setAuthorityPrefix("APPROLE_");

    JwtAuthenticationConverter result =
        new JwtAuthenticationConverter();

    result.setJwtGrantedAuthoritiesConverter(converter);

    return result;
}
