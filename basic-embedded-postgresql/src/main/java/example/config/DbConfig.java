package example.config;

import static java.lang.String.format;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

import javax.sql.DataSource;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.DependsOn;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import de.flapdoodle.embed.process.runtime.Network;

import ru.yandex.qatools.embed.postgresql.EmbeddedPostgres;
import ru.yandex.qatools.embed.postgresql.PostgresExecutable;
import ru.yandex.qatools.embed.postgresql.PostgresProcess;
import ru.yandex.qatools.embed.postgresql.PostgresStarter;
import ru.yandex.qatools.embed.postgresql.config.AbstractPostgresConfig;
import ru.yandex.qatools.embed.postgresql.config.PostgresConfig;
import ru.yandex.qatools.embed.postgresql.distribution.Version;

@Configuration
@EnableTransactionManagement
public class DbConfig {
	private static final List<String> DEFAULT_ADDITIONAL_INIT_DB_PARAMS = Arrays
			.asList("--nosync" /*, "--locale=en_US" */);

	/**
	 * @param config the PostgresConfig configuration which will be used to get the needed host, port..
	 * @return the created DB datasource
	 */
	@Bean
	@DependsOn("postgresProcess")
	public DataSource dataSource(PostgresConfig config) {

		DriverManagerDataSource ds = new DriverManagerDataSource();
		ds.setDriverClassName("org.postgresql.Driver");
		ds.setUrl(format("jdbc:postgresql://%s:%s/%s", config.net().host(),
				config.net().port(), config.storage().dbName()));
		ds.setUsername(config.credentials().username());
		ds.setPassword(config.credentials().password());
		return ds;
	}

	/**
	 * @return PostgresConfig that contains embedded db configuration like user name , password
	 * @throws IOException
	 */
	@Bean
	public PostgresConfig postgresConfig() throws IOException {
		// make it readable from configuration source file or system , it is hard
		// coded here for explanation purpose only
		final PostgresConfig postgresConfig = new PostgresConfig(Version.V10_6,
				new AbstractPostgresConfig.Net("localhost",
						Network.getFreeServerPort()),
				new AbstractPostgresConfig.Storage("test"),
				new AbstractPostgresConfig.Timeout(),
				new AbstractPostgresConfig.Credentials("user", "pass"));

		postgresConfig.getAdditionalInitDbParams()
				.addAll(DEFAULT_ADDITIONAL_INIT_DB_PARAMS);
		// postgresConfig.getAdditionalPostgresParams().add(cachedRuntimeConfig("/path/to/my/extracted/postgres")))
		return postgresConfig;
	}

	/**
	 * @param config the PostgresConfig configuration to use to start Postgres db process
	 * @return PostgresProcess , the started db process
	 * @throws IOException
	 */
	@Bean(destroyMethod = "stop")
	public PostgresProcess postgresProcess(PostgresConfig config)
			throws IOException {
		PostgresStarter<PostgresExecutable, PostgresProcess> runtime = PostgresStarter
				.getInstance(EmbeddedPostgres.cachedRuntimeConfig(
						Paths.get(System.getProperty("java.io.tmpdir"), "skp")));
		PostgresExecutable exec = runtime.prepare(config);
		PostgresProcess process = exec.start();
		return process;
	}
}
