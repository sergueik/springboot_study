package example.config;
@Configuration
@EnableTransactionManagement
@RequiredArgsConstructor
@EnableJpaRepositories(
        basePackages = {"com.example.database2.repository"},
        entityManagerFactoryRef = "database2EntityManagerFactory",
        transactionManagerRef = "database2EntityManagerFactory"

)
// origin: https://qna.habr.com/q/1343848
// NOTE: helps preventing runtime reflection error
// Error creating bean with name 'smpEntityManagerFactory' defined in class path resource [com/example/birthdays/configurationDBconnect/JpaSMPConfiguration.class]: Unable to create requested service [org.hibernate.engine.jdbc.env.spi.JdbcEnvironment] due to: Unable to determine Dialect without JDBC metadata (please set 'jakarta.persistence.jdbc.url' for common cases or 'hibernate.dialect' when a custom Dialect implementation must be provided)

public class Database2JpaConfig {

    private final Environment env;

    @Bean
    @Primary
    @ConfigurationProperties("spring.datasource.database2")
    public DataSourceProperties database2DataSourceProperties() {
        return new DataSourceProperties();
    }


    @Bean(name = "database2DataSource")
    @Primary
    public DataSource database2DataSource() {
        return database2DataSourceProperties()
                .initializeDataSourceBuilder()
                .build();
    }

    @Bean(name = "database2EntityManagerFactory")
    @Primary
    public LocalContainerEntityManagerFactoryBean database2EntityManagerFactory(
            @Qualifier("database2DataSource") DataSource dataSource,
            EntityManagerFactoryBuilder builder) {
        HashMap<String, Object> properties = new HashMap<>();
        properties.putAll(hibernateProperties());
        properties.putAll(enversProperties());

        return builder
                .dataSource(dataSource)
                .properties(properties)
                .packages("com.example.database2.model")
                .build();
    }

    /**
     * Hibernate properties
     * @return Map of properties for Hibernate
     */
    private Map<String, Object> hibernateProperties() {
        HashMap<String, Object> properties = new HashMap<>();
        properties.put("hibernate.hbm2ddl.auto", env.getProperty("spring.datasource.database2.jpa.hibernate.ddl-auto"));
        properties.put("hibernate.dialect", env.getProperty("spring.datasource.database2.jpa.database-platform"));
        properties.put("hibernate.globally_quoted_identifiers", env.getProperty("spring.datasource.database2.jpa.properties.hibernate.globally_quoted_identifiers"));
        properties.put("hibernate.globally_quoted_identifiers_skip_column_definitions", env.getProperty("spring.datasource.database2.jpa.properties.hibernate.globally_quoted_identifiers_skip_column_definitions"));
        properties.put("hibernate.default_schema", env.getProperty("spring.datasource.database2.jpa.properties.hibernate.default_schema"));
        properties.put("hibernate.format_sql", env.getProperty("spring.datasource.database2.jpa.properties.hibernate.format_sql"));
        properties.put("hibernate.show_sql", env.getProperty("spring.datasource.database2.jpa.show-sql"));
        properties.put("hibernate.order_inserts", env.getProperty("spring.datasource.database2.jpa.properties.hibernate.order_inserts"));
        properties.put("hibernate.order_updates", env.getProperty("spring.datasource.database2.jpa.properties.hibernate.order_updates"));
        properties.put("hibernate.jdbc.batch_size", env.getProperty("spring.datasource.database2.jpa.properties.hibernate.jdbc.batch_size"));
        properties.put("spring.jpa.open-in-view", env.getProperty("spring.datasource.database2.jpa.open-in-view"));
        properties.put("spring.jpa.generate-ddl", env.getProperty("spring.datasource.database2.jpa.generate-ddl"));
        return properties;
    }



    @Bean(name = "database2EntityManagerFactory")
    @Primary
    public PlatformTransactionManager database2EntityManagerFactory(
            @Qualifier("database2EntityManagerFactory") LocalContainerEntityManagerFactoryBean database2EntityManagerFactory) {
        return new JpaTransactionManager(Objects.requireNonNull(database2EntityManagerFactory.getObject()));
    }
}

