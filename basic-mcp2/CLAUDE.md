# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is "dvaas" (Dan Vega as a Service) - a Spring Boot 3.5.5 application that implements a Model Context Protocol (MCP) server using Spring AI and Anthropic Claude integration. The project demonstrates how to build AI-powered services with MCP capabilities in a Spring Boot environment, featuring integrations for YouTube, Blog (RSS), Speaking Engagements, Newsletter (Beehiiv), and Podcast (Transistor.fm).

## Build and Development Commands

### Maven Commands
- **Build the project**: `./mvnw clean compile`
- **Run tests**: `./mvnw test`
- **Run the application**: `./mvnw spring-boot:run`
- **Package as JAR**: `./mvnw package`
- **Clean build artifacts**: `./mvnw clean`

### Application Commands
- **Run specific test class**: `./mvnw test -Dtest=ApplicationTests`
- **Run with profile**: `./mvnw spring-boot:run -Dspring.profiles.active=dev`
- **Test MCP server**: Ensure ANTHROPIC_API_KEY, YOUTUBE_API_KEY, and YOUTUBE_CHANNEL_ID environment variables are set

## Architecture

### Technology Stack
- **Java 24** (preview features enabled)
- **Spring Boot 3.5.5** with Spring Web starter
- **Spring AI 1.1.0-M1** with Anthropic integration
- **Spring AI MCP Server WebMVC** for Model Context Protocol capabilities
- **Google YouTube Data API v3** for YouTube integration
- **Rome RSS Library v2.1.0** for RSS feed parsing
- **Beehiiv API** for newsletter management
- **Transistor.fm API** for podcast management
- **Jakarta Bean Validation** for configuration validation
- **Maven** for build management

### Project Structure
```
src/
├── main/java/dev/danvega/dvaas/
│   ├── Application.java               # Main Spring Boot application class
│   ├── config/
│   │   ├── DvaasConfiguration.java    # Main configuration class
│   │   ├── BlogProperties.java        # Blog configuration properties
│   │   ├── YouTubeProperties.java     # YouTube configuration properties
│   │   ├── SpeakingProperties.java    # Speaking configuration properties
│   │   ├── NewsletterProperties.java  # Newsletter configuration properties
│   │   └── PodcastProperties.java     # Podcast configuration properties
│   └── tools/
│       ├── blog/
│       │   ├── BlogTools.java         # MCP tools for blog operations
│       │   ├── BlogService.java       # RSS feed service layer
│       │   └── model/                 # Blog domain models
│       ├── youtube/
│       │   ├── YouTubeTools.java      # MCP tools for YouTube operations
│       │   ├── YouTubeService.java    # YouTube Data API service layer
│       │   └── model/                 # YouTube domain models
│       ├── speaking/
│       │   ├── SpeakingTools.java     # MCP tools for speaking engagements
│       │   ├── SpeakingService.java   # Speaking data service layer
│       │   └── model/                 # Speaking domain models
│       ├── newsletter/
│       │   ├── NewsletterTools.java   # MCP tools for newsletter operations
│       │   ├── NewsletterService.java # Beehiiv API service layer
│       │   └── model/                 # Newsletter domain models
│       └── podcast/
│           ├── PodcastTools.java      # MCP tools for podcast operations
│           ├── PodcastService.java    # Transistor.fm API service layer
│           └── model/                 # Podcast domain models
├── main/resources/
│   └── application.properties         # Application and MCP server configuration
└── test/java/dev/danvega/dvaas/
    ├── ApplicationTests.java          # Basic application tests
    └── tools/
        ├── blog/                      # Blog tools tests
        ├── youtube/                   # YouTube tools tests
        ├── speaking/                  # Speaking tools tests
        ├── newsletter/                # Newsletter tools tests
        └── podcast/                   # Podcast tools tests
```

### Configuration

#### Main Configuration
- Main configuration is in `application.properties`
- Application name: "dvaas"
- Anthropic API key configuration via environment variable
- MCP server configuration (enabled, named "dvaas-mcp-server", sync type, streamable protocol)

#### Configuration Properties Classes
The application uses strongly-typed configuration properties instead of raw strings:

- **BlogProperties** (`dev.danvega.dvaas.config.BlogProperties`)
  - `dvaas.blog.rss-url`: RSS feed URL for the blog
  - `dvaas.blog.cache-duration`: Cache duration for RSS data (default: PT30M)

- **YouTubeProperties** (`dev.danvega.dvaas.config.YouTubeProperties`)
  - `dvaas.youtube.api-key`: YouTube Data API key (via environment variable)
  - `dvaas.youtube.channel-id`: YouTube channel ID (via environment variable)
  - `dvaas.youtube.application-name`: Application name for API requests

- **SpeakingProperties** (`dev.danvega.dvaas.config.SpeakingProperties`)
  - `dvaas.speaking.api-url`: Speaking API endpoint URL
  - `dvaas.speaking.cache-duration`: Cache duration for speaking data (default: PT30M)

- **NewsletterProperties** (`dev.danvega.dvaas.config.NewsletterProperties`)
  - `dvaas.newsletter.api-key`: Beehiiv API key (via environment variable)
  - `dvaas.newsletter.base-url`: Base URL for Beehiiv API (default: https://api.beehiiv.com/v2)
  - `dvaas.newsletter.cache-duration`: Cache duration for newsletter data (default: PT30M)
  - `dvaas.newsletter.publications`: Map of publication names to publication IDs (e.g., danvega, bytesizedai)

- **PodcastProperties** (`dev.danvega.dvaas.config.PodcastProperties`)
  - `dvaas.podcast.api-key`: Transistor.fm API key (via environment variable)
  - `dvaas.podcast.application-name`: Application name for API requests (default: dvaas-podcast-mcp)
  - `dvaas.podcast.cache-duration`: Cache duration for podcast data (default: PT30M)
  - `dvaas.podcast.spring-office-hours-show-id`: Show ID for Spring Office Hours podcast
  - `dvaas.podcast.fundamentals-show-id`: Show ID for Fundamentals of Software Engineering podcast

#### Configuration Validation
The application includes Jakarta Bean Validation for configuration properties:
- **BlogProperties validation**:
  - `@NotBlank` for RSS URL (required, non-empty)
  - `@Pattern` for URL format validation (must start with http:// or https://)
  - `@NotNull` for cache duration with minimum 1-minute validation
- **YouTubeProperties validation**:
  - `@NotBlank` for API key and channel ID (required, non-empty)
  - `@Pattern` for channel ID format (24 characters starting with UC, UU, or HC)
  - API key length validation (10-100 characters)
- **SpeakingProperties validation**:
  - `@NotBlank` for API URL (required, non-empty)
  - `@Pattern` for URL format validation (must start with http:// or https://)
  - `@NotNull` for cache duration with minimum 1-minute validation
- **NewsletterProperties validation**:
  - `@NotBlank` for API key (required, non-empty)
  - `@Pattern` for base URL format validation (must start with http:// or https://)
  - `@NotNull` for cache duration with minimum 1-minute validation
  - Publications map must contain at least one publication with non-blank IDs
- **PodcastProperties validation**:
  - `@NotBlank` for API key (required, non-empty)
  - API key length validation (10-100 characters)
  - `@NotNull` for cache duration with minimum 1-minute validation
  - Show ID resolution for known podcast names (Spring Office Hours, Fundamentals)

#### Benefits of Configuration Properties
- **Type Safety**: Compile-time validation instead of runtime string errors
- **IDE Support**: Auto-completion and validation in IDEs
- **Documentation**: Built-in property documentation
- **Default Values**: Centralized default value management
- **Runtime Validation**: Jakarta Bean Validation ensures correct configuration at startup

### Dependencies
- Spring Boot Web Starter (REST/Web services)
- Spring Boot Validation Starter (Jakarta Bean Validation support)
- Spring AI Anthropic integration (for AI model interactions)
- Spring AI MCP Server WebMVC (for Model Context Protocol server capabilities)
- Google YouTube Data API v3 (for YouTube integration)
- Google API client libraries (for HTTP transport and JSON processing)
- Rome RSS Library v2.1.0 (for RSS feed parsing)
- Standard Spring Boot testing dependencies

## Key Points for Development
- Functional MCP server with 5 integration implementations (YouTube, Blog, Speaking, Newsletter, Podcast)
- Uses Spring AI Community MCP annotations (@McpTool, @McpToolParam)
- **Strongly-typed configuration properties** instead of raw strings for better maintainability
- Implements **21 MCP tools total** across 5 feature areas:

### YouTube MCP Tools (4 tools):
  - `youtube-get-latest-videos`: Get recent videos from Dan Vega's channel
  - `youtube-get-top-videos`: Get top performing videos by view count
  - `youtube-search-videos-by-topic`: Search videos by keyword/topic
  - `youtube-get-channel-stats`: Get channel statistics and information

### Blog MCP Tools (4 tools):
  - `blog-get-latest-posts`: Get recent blog posts from RSS feed
  - `blog-search-posts-by-keyword`: Search posts by keyword in title/description
  - `blog-get-posts-by-date-range`: Filter posts by date ranges or specific years
  - `blog-get-stats`: Get comprehensive blog statistics and metrics

### Speaking MCP Tools (4 tools):
  - `speaking-get-latest-engagements`: Get recent speaking engagements
  - `speaking-get-upcoming-events`: Get upcoming speaking events
  - `speaking-search-by-topic`: Search speaking engagements by topic/keyword
  - `speaking-get-stats`: Get overall speaking statistics

### Newsletter MCP Tools (4 tools):
  - `newsletter-get-latest-posts`: Get recent newsletter posts (supports danvega, bytesizedai publications)
  - `newsletter-search-posts-by-keyword`: Search posts by keyword in title/content/authors
  - `newsletter-get-posts-by-status`: Filter posts by status (draft, confirmed, archived)
  - `newsletter-get-publication-stats`: Get newsletter publication statistics

### Podcast MCP Tools (5 tools):
  - `podcast-get-shows`: Get all podcast shows (Spring Office Hours, Fundamentals of Software Engineering)
  - `podcast-get-latest-episodes`: Get recent episodes across all shows or filtered by show
  - `podcast-search-episodes`: Search episodes by keyword in title/description
  - `podcast-get-episode-details`: Get detailed information about a specific episode
  - `podcast-get-stats`: Get overall podcast statistics with per-show summaries

- Uses Java 24 with latest Spring Boot features
- Ready for extending with additional MCP tools following the established patterns
- Comprehensive test coverage with proper package organization
- Conditional bean loading based on available configuration (tools only load when properly configured)

## Spring AI Documentation

When asked about Spring AI features, configuration, APIs, or implementation details, use the Context7 MCP server to fetch the most current documentation:

### Instructions for Claude Code:
1. **Use Context7 MCP** to access Spring AI 1.1.0-M1 documentation
2. **Library ID**: `/spring-projects/spring-ai/v1_1_0_m1`
3. **Focus areas** relevant to this project:
   - Anthropic Claude integration and chat models
   - Chat client configuration and usage
   - **MCP Server implementation** with Spring AI Community annotations
   - **MCP Tools development** (@McpTool, @McpToolParam patterns)
   - MCP server configuration and deployment
   - Embedding models and vector stores
   - Retrieval Augmented Generation (RAG) patterns
   - Spring Boot auto-configuration for MCP servers

### When to use Spring AI documentation:
- Implementing AI-powered endpoints or services
- Configuring Anthropic Claude chat models
- **Creating new MCP tools** following YouTubeTools pattern
- **MCP server configuration** and troubleshooting
- Setting up vector stores or embeddings
- Troubleshooting Spring AI integration issues
- Adding new AI capabilities (audio, image, etc.)
- Understanding Spring AI MCP server best practices and patterns