package example.config;
@Configuration
@EnableTransactionManagement
@RequiredArgsConstructor
@EnableJpaRepositories(
        basePackages = {"com.example.database1.repository"},
        entityManagerFactoryRef = "database1EntityManagerFactory",
        transactionManagerRef = "database1EntityManagerFactory"

)
// origin: https://qna.habr.com/q/1343848
public class Database1JpaConfig {

    private final Environment env;

    @Bean
    @Primary
    @ConfigurationProperties("spring.datasource.database1")
    public DataSourceProperties database1DataSourceProperties() {
        return new DataSourceProperties();
    }


    @Bean(name = "database1DataSource")
    @Primary
    public DataSource database1DataSource() {
        return database1DataSourceProperties()
                .initializeDataSourceBuilder()
                .build();
    }

    @Bean(name = "database1EntityManagerFactory")
    @Primary
    public LocalContainerEntityManagerFactoryBean database1EntityManagerFactory(
            @Qualifier("database1DataSource") DataSource dataSource,
            EntityManagerFactoryBuilder builder) {
        HashMap<String, Object> properties = new HashMap<>();
        properties.putAll(hibernateProperties());
        properties.putAll(enversProperties());

        return builder
                .dataSource(dataSource)
                .properties(properties)
                .packages("com.example.database1.model")
                .build();
    }

    /**
     * Hibernate properties
     * @return Map of properties for Hibernate
     */
    private Map<String, Object> hibernateProperties() {
        HashMap<String, Object> properties = new HashMap<>();
        properties.put("hibernate.hbm2ddl.auto", env.getProperty("spring.datasource.database1.jpa.hibernate.ddl-auto"));
        properties.put("hibernate.dialect", env.getProperty("spring.datasource.database1.jpa.database-platform"));
        properties.put("hibernate.globally_quoted_identifiers", env.getProperty("spring.datasource.database1.jpa.properties.hibernate.globally_quoted_identifiers"));
        properties.put("hibernate.globally_quoted_identifiers_skip_column_definitions", env.getProperty("spring.datasource.database1.jpa.properties.hibernate.globally_quoted_identifiers_skip_column_definitions"));
        properties.put("hibernate.default_schema", env.getProperty("spring.datasource.database1.jpa.properties.hibernate.default_schema"));
        properties.put("hibernate.format_sql", env.getProperty("spring.datasource.database1.jpa.properties.hibernate.format_sql"));
        properties.put("hibernate.show_sql", env.getProperty("spring.datasource.database1.jpa.show-sql"));
        properties.put("hibernate.order_inserts", env.getProperty("spring.datasource.database1.jpa.properties.hibernate.order_inserts"));
        properties.put("hibernate.order_updates", env.getProperty("spring.datasource.database1.jpa.properties.hibernate.order_updates"));
        properties.put("hibernate.jdbc.batch_size", env.getProperty("spring.datasource.database1.jpa.properties.hibernate.jdbc.batch_size"));
        properties.put("spring.jpa.open-in-view", env.getProperty("spring.datasource.database1.jpa.open-in-view"));
        properties.put("spring.jpa.generate-ddl", env.getProperty("spring.datasource.database1.jpa.generate-ddl"));
        return properties;
    }



    @Bean(name = "database1EntityManagerFactory")
    @Primary
    public PlatformTransactionManager database1EntityManagerFactory(
            @Qualifier("database1EntityManagerFactory") LocalContainerEntityManagerFactoryBean database1EntityManagerFactory) {
        return new JpaTransactionManager(Objects.requireNonNull(database1EntityManagerFactory.getObject()));
    }
}

