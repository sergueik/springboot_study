FROM builder

# install the published application sans markitdown-ocr and openai
ARG VERSION=0.1.8

RUN python -m pip install --no-cache-dir --upgrade pip \
    && python -m pip install --no-cache-dir \
        "markitdown[all]==${VERSION}" \
        markitdown-sample-plugin
        
COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

WORKDIR /data

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
CMD ["--help"]
