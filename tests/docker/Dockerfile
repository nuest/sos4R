# See https://github.com/52North/SOS/issues/842#issuecomment-641746547
FROM 52north/sos:5.1.0

RUN wget -O /etc/sos/configuration.json https://raw.githubusercontent.com/52North/SOS/develop/docker/default-config/configuration.json
RUN wget -O /etc/sos/datasource.properties https://raw.githubusercontent.com/52North/SOS/develop/docker/default-config/datasource.properties

### Build:
# docker build --tag containeritsos:test .
# Run:
# docker run -p 8080:8080 containeritsos:test
# Run in R:
#