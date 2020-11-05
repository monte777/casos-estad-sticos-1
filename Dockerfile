FROM registry.access.redhat.com/rhel7-atomic

ENV LC_ALL=en_US.UTF-8

RUN microdnf update
RUN microdnf install gcc gcc-c++ gcc-gfortran --enablerepo=rhel-7-server-rpms
RUN microdnf clean all
RUN microdnf install bison byacc cscope ctags cvs diffstat doxygen --enablerepo=rhel-7-server-rpms
RUN microdnf clean all
RUN microdnf install fontconfig --enablerepo=rhel-7-server-rpms
RUN microdnf clean all
RUN microdnf install flex gettext indent intltool libtool --enablerepo=rhel-7-server-rpms
RUN microdnf clean all
RUN microdnf install patch patchutils rcs swig systemtap --enablerepo=rhel-7-server-rpms
RUN microdnf clean all
RUN microdnf install libcurl libcurl-devel openssl-devel libxml2-devel --enablerepo=rhel-7-server-rpms
RUN microdnf clean all
RUN microdnf install java-1.8.0-openjdk-headless --enablerepo=rhel-7-server-rpms 
RUN microdnf clean all
RUN microdnf install java-devel --enablerepo=rhel-7-server-rpms 
RUN microdnf clean all
RUN microdnf install wget --enablerepo=rhel-7-server-rpms 
RUN microdnf clean all
RUN wget https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm 
RUN rpm -Uvh epel-release-latest-7*.rpm
RUN microdnf update
RUN microdnf install R --enablerepo=epel --enablerepo=rhel-7-server-optional-rpms --enablerepo=rhel-7-server-rpms

RUN R -e "install.packages('devtools', repos = 'https://cloud.r-project.org')"
RUN R -e "devtools::install_version('plumber', version = '0.4.4', repos = 'https://cloud.r-project.org')"
RUN R -e "devtools::install_version('dplyr', version = '0.7.5', repos = 'https://cloud.r-project.org')"
RUN R -e "devtools::install_version('jsonlite', version = '1.5', repos = 'https://cloud.r-project.org')"
RUN R -e "devtools::install_version('caret', version = '6.0-78', repos = 'https://cloud.r-project.org')"
RUN R -e "devtools::install_version('Iso', version = '0.0-17', repos = 'https://cloud.r-project.org')"
RUN R -e "devtools::install_version('e1071', version = '1.6-8', repos = 'https://cloud.r-project.org')"
RUN R -e "devtools::install_version('readr', version = '1.1.1', repos = 'https://cloud.r-project.org')"
RUN R -e "devtools::install_version('scales', version = '1.1.1', repos = 'https://cloud.r-project.org')"

ENV LANG=en_US.utf-8
ENV LC_ALL=en_US.UTF-8

COPY . /app
WORKDIR /app

EXPOSE 8080
ENTRYPOINT ["Rscript"]
CMD ["arranca.R"]
