# Test Structure

This document outlines the refactored test structure that mirrors the source code package organization.

## Directory Structure

```
src/test/java/dev/danvega/dvaas/
├── ApplicationTests.java               # Basic application tests
└── tools/
    ├── blog/
    │   ├── BlogServiceTest.java        # Unit tests for BlogService
    │   ├── BlogToolsTest.java          # Unit tests for BlogTools MCP class
    │   ├── BlogServiceIntegrationTest.java  # Integration tests with real RSS feed
    │   └── BlogIntegrationTest.java    # Spring Boot integration test for blog functionality
    └── youtube/
        ├── YouTubeServiceTest.java     # Unit tests for YouTubeService
        ├── YouTubeToolsTest.java       # Unit tests for YouTubeTools MCP class
        └── YouTubeServiceIntegrationTest.java  # Integration tests with real YouTube API
```

## Test Categories

### Unit Tests
- **BlogServiceTest**: Tests BlogService logic, model classes, and error handling
- **BlogToolsTest**: Tests BlogTools MCP methods with mocked dependencies
- **YouTubeServiceTest**: Tests YouTubeService logic and model classes
- **YouTubeToolsTest**: Tests YouTubeTools MCP methods with mocked dependencies

### Integration Tests
- **BlogServiceIntegrationTest**: Tests with real RSS feed (requires `BLOG_INTEGRATION_TEST=true`)
- **YouTubeServiceIntegrationTest**: Tests with real YouTube API (requires API keys)
- **BlogIntegrationTest**: Spring Boot integration test for blog service and tools

### Application Tests
- **ApplicationTests**: Basic Spring Boot application startup test

## Running Tests

### Run all tests
```bash
./mvnw test
```

### Run tests by package
```bash
# Blog tests only
./mvnw test -Dtest="dev.danvega.dvaas.tools.blog.*"

# YouTube tests only
./mvnw test -Dtest="dev.danvega.dvaas.tools.youtube.*"
```

### Run specific test classes
```bash
# Blog service unit tests
./mvnw test -Dtest=dev.danvega.dvaas.tools.blog.BlogServiceTest

# Blog integration test
./mvnw test -Dtest=dev.danvega.dvaas.tools.blog.BlogIntegrationTest
```

### Run integration tests with real APIs
```bash
# Blog RSS integration (no API keys needed)
BLOG_INTEGRATION_TEST=true ./mvnw test -Dtest=dev.danvega.dvaas.tools.blog.BlogServiceIntegrationTest

# YouTube integration (requires API keys)
YOUTUBE_API_KEY=your_key YOUTUBE_CHANNEL_ID=your_channel ./mvnw test -Dtest=dev.danvega.dvaas.tools.youtube.YouTubeServiceIntegrationTest
```

## Benefits of This Structure

1. **Mirror Source Code**: Test packages mirror the source code structure
2. **Better Organization**: Related tests are grouped together
3. **Easier Navigation**: Easy to find tests for specific components
4. **Clear Separation**: Unit tests, integration tests, and application tests are clearly separated
5. **Maintainability**: Easier to maintain and refactor when code changes
6. **IDE Support**: Better IDE support for running tests by package