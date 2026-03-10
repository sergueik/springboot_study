

# DVaaS - Dan Vega as a Service

A Spring Boot 3.5.5 application that implements a Model Context Protocol (MCP) server using Spring AI and Anthropic Claude integration. This project demonstrates how to build AI-powered services with MCP capabilities in a Spring Boot environment.

## Features

- **MCP Server**: Implements Model Context Protocol server functionality
- **Spring AI Integration**: Uses Spring AI 1.1.0-M1 with Anthropic Claude
- **YouTube Integration**: Provides MCP tools for YouTube channel operations and video management
- **Blog Integration**: Provides MCP tools for RSS feed parsing and blog post management
- **Speaking Integration**: Provides MCP tools for managing speaking engagements and events
- **Newsletter Integration**: Provides MCP tools for newsletter management via Beehiiv API
- **Podcast Integration**: Provides MCP tools for podcast management via Transistor.fm API
- **Configuration Validation**: Jakarta Bean Validation for robust configuration management
- **Java 24**: Utilizes the latest Java features with preview support

## Prerequisites

- Java 24
- Maven 3.6+
- Anthropic API Key (required)
- YouTube Data API Key (optional - for YouTube features)
- YouTube Channel ID (optional - for YouTube features)
- RSS Feed URL (optional - for blog features, configured in application.properties)
- Speaking API URL (optional - for speaking features)
- Beehiiv API Key (optional - for newsletter features)
- Transistor.fm API Key (optional - for podcast features)

## Setup

1. **Clone the repository**
   ```bash
   git clone <repository-url>
   cd dvaas
   ```

2. **Set up environment variables**
   ```bash
   # Required
   export ANTHROPIC_API_KEY=your_anthropic_api_key_here

   # Optional - YouTube features
   export YOUTUBE_API_KEY=your_youtube_api_key_here
   export YOUTUBE_CHANNEL_ID=your_youtube_channel_id_here

   # Optional - Newsletter features
   export BEEHIIV_API_KEY=your_beehiiv_api_key_here

   # Optional - Podcast features
   export TRANSISTOR_API_KEY=your_transistor_api_key_here

   # Blog RSS URL and Speaking API URL are configured in application.properties
   ```

3. **Build the project**
   ```bash
   ./mvnw clean compile
   ```

4. **Run the application**
   ```bash
   ./mvnw spring-boot:run
   ```

## MCP Server Configuration

The application is configured as an MCP server with the following settings:

- **Server Name**: `dvaas-mcp-server`
- **Version**: `0.0.1`
- **Type**: `SYNC`
- **Protocol**: `streamable`

## Available MCP Tools

The application provides **21 MCP tools** organized by feature area:

### 🎥 YouTube Tools (4 tools)

Tools for YouTube channel operations and video management.

#### youtube-get-latest-videos
Get the most recent videos from Dan Vega's YouTube channel.

**Parameters:**
- `count` (optional): Number of videos to retrieve (default: 10, max: 50)

#### youtube-get-top-videos
Get the top-performing videos from Dan Vega's YouTube channel by view count.

**Parameters:**
- `count` (optional): Number of videos to retrieve (default: 10, max: 50)
- `timeRange` (optional): Time range filter - 'recent', 'month', 'year', 'all' (default: 'recent')

#### youtube-search-videos-by-topic
Search for videos on Dan Vega's YouTube channel by topic or keyword.

**Parameters:**
- `topic` (required): Topic or keyword to search for (e.g., 'java', 'spring', 'spring-ai')
- `count` (optional): Number of videos to retrieve (default: 10, max: 50)

#### youtube-get-channel-stats
Get overall statistics and information about Dan Vega's YouTube channel.

**Parameters:** None

### 📝 Blog Tools (4 tools)

Tools for RSS feed parsing and blog post management.

#### blog-get-latest-posts
Get the most recent blog posts from Dan Vega's RSS feed.

**Parameters:**
- `count` (required): Number of posts to retrieve (max: 50)

#### blog-search-posts-by-keyword
Search blog posts by keyword in title and description.

**Parameters:**
- `keyword` (required): Keyword to search for (e.g., 'spring', 'java', 'ai')
- `maxResults` (required): Maximum number of results to return (max: 50)

#### blog-get-posts-by-date-range
Get blog posts within a specific date range or year.

**Parameters:**
- `dateRange` (required): Date range in format 'YYYY' for year or 'YYYY-MM-DD to YYYY-MM-DD' for custom range
- `maxResults` (required): Maximum number of results to return (max: 50)

#### blog-get-stats
Get comprehensive statistics about the blog including total posts, posting frequency, and trends.

**Parameters:** None

### 🎤 Speaking Tools (4 tools)

Tools for managing speaking engagements and events.

#### speaking-get-latest-engagements
Get the most recent speaking engagements from Dan Vega's speaking schedule.

**Parameters:**
- `count` (optional): Number of engagements to retrieve (default: 10, max: 50)

#### speaking-get-upcoming-events
Get upcoming speaking events from Dan Vega's speaking schedule.

**Parameters:**
- `count` (optional): Number of events to retrieve (default: 10, max: 50)

#### speaking-search-by-topic
Search for speaking engagements by topic or keyword (e.g., 'spring', 'ai', 'java', 'microservices').

**Parameters:**
- `topic` (required): Topic or keyword to search for in titles, descriptions, or event names
- `count` (optional): Number of engagements to retrieve (default: 10, max: 50)

#### speaking-get-stats
Get overall statistics and information about Dan Vega's speaking engagements.

**Parameters:** None

### 📰 Newsletter Tools (4 tools)

Tools for newsletter management via Beehiiv API. Supports multiple publications (danvega, bytesizedai).

#### newsletter-get-latest-posts
Get the most recent newsletter posts from Dan Vega's publications.

**Parameters:**
- `publication` (optional): Publication name - 'danvega', 'bytesizedai', or 'all' (default: 'all')
- `count` (optional): Number of posts to retrieve (default: 10, max: 50)

#### newsletter-search-posts-by-keyword
Search for newsletter posts by keyword in title, content, or authors.

**Parameters:**
- `keyword` (required): Keyword to search for (e.g., 'spring', 'ai', 'java')
- `publication` (optional): Publication name - 'danvega', 'bytesizedai', or 'all' (default: 'all')
- `count` (optional): Number of posts to retrieve (default: 10, max: 50)

#### newsletter-get-posts-by-status
Get newsletter posts filtered by status.

**Parameters:**
- `status` (optional): Post status - 'draft', 'confirmed', 'archived', or 'all' (default: 'confirmed')
- `publication` (optional): Publication name - 'danvega', 'bytesizedai', or 'all' (default: 'all')
- `count` (optional): Number of posts to retrieve (default: 10, max: 50)

#### newsletter-get-publication-stats
Get statistics and information about Dan Vega's newsletter publications.

**Parameters:**
- `publication` (optional): Publication name - 'danvega', 'bytesizedai', or 'all' (default: 'all')

### 🎙️ Podcast Tools (5 tools)

Tools for podcast management via Transistor.fm API.

#### podcast-get-shows
Get all podcast shows hosted by Dan Vega on Transistor.fm.

**Parameters:** None

#### podcast-get-latest-episodes
Get the most recent podcast episodes across all shows or filtered by show name/ID.

**Parameters:**
- `count` (optional): Number of episodes to retrieve (default: 10, max: 50)
- `show` (optional): Filter by show name (e.g., 'Spring Office Hours', 'Fundamentals of Software Engineering') or show ID

#### podcast-search-episodes
Search for podcast episodes by keyword in title or description.

**Parameters:**
- `keyword` (required): Keyword to search for in episode titles and descriptions
- `count` (optional): Number of episodes to retrieve (default: 10, max: 50)
- `show` (optional): Filter by show name or show ID

#### podcast-get-episode-details
Get detailed information about a specific podcast episode by its ID.

**Parameters:**
- `episodeId` (required): Episode ID to retrieve

#### podcast-get-stats
Get overall statistics and information about Dan Vega's podcasts, including episode counts, publishing frequency, and per-show summaries.

**Parameters:** None

### 🔮 Future Tools

The architecture is designed to easily support additional tool categories such as:
- **Social Media Tools**: Twitter/X integration, LinkedIn posts
- **Course Tools**: Course information and management
- **Analytics Tools**: Cross-platform analytics and insights
- **Community Tools**: Discord/Slack integration, community metrics

## Development

### Build Commands

```bash
# Clean and compile
./mvnw clean compile

# Run tests
./mvnw test

# Package as JAR
./mvnw package

# Run with specific profile
./mvnw spring-boot:run -Dspring.profiles.active=dev
```

### Testing

```bash
# Run all tests
./mvnw test

# Run specific test class
./mvnw test -Dtest=ApplicationTests
```

## Technology Stack

- **Java 24** (preview features enabled)
- **Spring Boot 3.5.5** with Spring Web starter
- **Spring AI 1.1.0-M1** with Anthropic integration
- **Spring AI MCP Server WebMVC** for MCP capabilities
- **Google YouTube Data API v3** for YouTube integration
- **Rome RSS Library v2.1.0** for RSS feed parsing
- **Jakarta Bean Validation** for configuration validation
- **Maven** for build management

## Project Structure

```
src/
├── main/java/dev/danvega/dvaas/
│   ├── Application.java              # Main Spring Boot application class
│   ├── config/
│   │   ├── DvaasConfiguration.java   # Main configuration class
│   │   ├── BlogProperties.java       # Blog configuration properties
│   │   ├── YouTubeProperties.java    # YouTube configuration properties
│   │   ├── SpeakingProperties.java   # Speaking configuration properties
│   │   ├── NewsletterProperties.java # Newsletter configuration properties
│   │   └── PodcastProperties.java    # Podcast configuration properties
│   └── tools/
│       ├── blog/
│       │   ├── BlogTools.java        # MCP tools for blog operations
│       │   ├── BlogService.java      # RSS feed service layer
│       │   └── model/                # Blog domain models
│       ├── youtube/
│       │   ├── YouTubeTools.java     # MCP tools for YouTube operations
│       │   ├── YouTubeService.java   # YouTube Data API service layer
│       │   └── model/                # YouTube domain models
│       ├── speaking/
│       │   ├── SpeakingTools.java    # MCP tools for speaking engagements
│       │   ├── SpeakingService.java  # Speaking data service layer
│       │   └── model/                # Speaking domain models
│       ├── newsletter/
│       │   ├── NewsletterTools.java  # MCP tools for newsletter operations
│       │   ├── NewsletterService.java # Beehiiv API service layer
│       │   └── model/                # Newsletter domain models
│       └── podcast/
│           ├── PodcastTools.java     # MCP tools for podcast operations
│           ├── PodcastService.java   # Transistor.fm API service layer
│           └── model/                # Podcast domain models
├── main/resources/
│   └── application.properties        # Application and MCP server configuration
└── test/java/dev/danvega/dvaas/
    ├── ApplicationTests.java         # Basic application tests
    └── tools/
        ├── blog/                     # Blog tools tests
        ├── youtube/                  # YouTube tools tests
        ├── speaking/                 # Speaking tools tests
        ├── newsletter/               # Newsletter tools tests
        └── podcast/                  # Podcast tools tests
```

## Configuration

The application uses **strongly-typed configuration properties** with validation for better maintainability and error prevention.

### Core Configuration

Key configuration properties in `application.properties`:

```properties
spring.application.name=dvaas
spring.ai.anthropic.api-key=${ANTHROPIC_API_KEY}

# MCP Server Configuration
spring.ai.mcp.server.enabled=true
spring.ai.mcp.server.name=dvaas-mcp-server
spring.ai.mcp.server.version=0.0.1
spring.ai.mcp.server.type=SYNC
spring.ai.mcp.server.protocol=streamable

# YouTube Configuration (optional)
dvaas.youtube.api-key=${YOUTUBE_API_KEY}
dvaas.youtube.channel-id=${YOUTUBE_CHANNEL_ID}
dvaas.youtube.application-name=dvaas-youtube-mcp

# Blog Configuration
dvaas.blog.rss-url=https://www.danvega.dev/rss.xml
dvaas.blog.cache-duration=PT30M
```

### Configuration Properties

All tool integrations use strongly-typed configuration properties with Jakarta Bean Validation:

- **BlogProperties**: RSS URL, cache duration
- **YouTubeProperties**: API key, channel ID, application name
- **SpeakingProperties**: API URL, cache duration
- **NewsletterProperties**: API key, base URL, cache duration, publication mappings
- **PodcastProperties**: API key, application name, cache duration, show IDs

Each configuration class includes:
- Runtime validation (format, required fields, ranges)
- Sensible defaults where applicable
- Helper methods for common operations
- Startup validation that prevents application launch with invalid configuration

### Feature-based Configuration

Tools are conditionally loaded based on available configuration:
- **YouTube tools**: Load when `dvaas.youtube.api-key` is configured
- **Blog tools**: Load when `dvaas.blog.rss-url` is configured
- **Speaking tools**: Load when `dvaas.speaking.api-url` is configured
- **Newsletter tools**: Load when `dvaas.newsletter.api-key` is configured
- **Podcast tools**: Load when `dvaas.podcast.api-key` is configured

Each tool category can be enabled/disabled independently.
