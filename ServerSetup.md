# Steps to install a new production server for http://code.world

These steps *only* apply to the main production server, and should not be used for third-party installations.

1. `sudo apt-get install git`
2. `git clone git@github.com:google/codeworld.git`
3. `./install.sh'
4. `echo 94846197422-jnkt1qd737993e7llrfa5pb1bqc72nog.apps.googleusercontent.com > web/clientId.txt`
5. `sudo apt-get install nginx`
6. Copy the SSL certificates into `/etc/nginx/ssl`, as follows:

        $ ls /etc/nginx/ssl/
        codeworld.crt  codeworld.csr  codeworld-info.crt  codeworld-info.csr  codeworld-info.key  codeworld.key

7. Copy the nginx config file to `/etc/nginx/sites/available/default`:

        upstream gameserver {
            server 127.0.0.1:9160;
        }

        server {
            listen 80;
            server_name codeworld.info www.codeworld.info code.world www.code.world;
            return 301 https://code.world$request_uri;
        }

        server {
            listen 443;
            server_name codeworld.info www.codeworld.info;

            ssl_certificate           /etc/nginx/ssl/codeworld-info.crt;
            ssl_certificate_key       /etc/nginx/ssl/codeworld-info.key;
            ssl on;
            ssl_session_cache  builtin:1000  shared:SSL:10m;
            ssl_protocols  TLSv1 TLSv1.1 TLSv1.2;
            ssl_ciphers HIGH:!aNULL:!eNULL:!EXPORT:!CAMELLIA:!DES:!MD5:!PSK:!RC4;
            ssl_prefer_server_ciphers on;

            return 301 https://code.world$request_uri;
        }

        server {
            listen 443;
            server_name code.world www.code.world;

            ssl_certificate           /etc/nginx/ssl/codeworld.crt;
            ssl_certificate_key       /etc/nginx/ssl/codeworld.key;
            ssl on;
            ssl_session_cache  builtin:1000  shared:SSL:10m;
            ssl_protocols  TLSv1 TLSv1.1 TLSv1.2;
            ssl_ciphers HIGH:!aNULL:!eNULL:!EXPORT:!CAMELLIA:!DES:!MD5:!PSK:!RC4;
            ssl_prefer_server_ciphers on;

            root /home/cdsmith/cw-web;
            error_page 404 = /404.html;

            location /gameserver {
                proxy_pass http://gameserver;

                # Enables WS support
                proxy_http_version 1.1;
                proxy_set_header Upgrade $http_upgrade;
                proxy_set_header Connection "upgrade";
                proxy_redirect off;
            }

            location / {
                try_files $uri $uri/index.html @cwserver;
            }

            location @cwserver {
                proxy_pass http://localhost:8080;

                proxy_intercept_errors on;

                proxy_set_header X-Real-IP $remote_addr;
                proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                proxy_set_header Host $http_host;
                proxy_set_header X-NginX-Proxy true;
            }
        }

        server {
            listen 80;
            server_name local-stackdriver-agent.stackdriver.com;
            location /nginx_status {
                stub_status on;
                access_log off;
                allow 127.0.0.1;
                deny all;
            }
            location / {
                root /dev/null;
            }
        }

8. `sudo nginx -s reload`
9. Add the following lines to `/etc/fstab`:

        /dev/sdb                /var/local/codeworld ext4      nofail
        /dev/sdc                /var/local/codeworld ext4      nofail

10. `sudo mkdir /var/local/codeworld`
11. `sudo mount /var/local/codeworld`
12. Verify that the mount succeeded and the data is available with `df` and `ls /var/local/codeworld`.
13. Copy the following to `/etc/init.d/codeworld` and ensure the file is owned by root and executable:

        #!/bin/sh
        # kFreeBSD do not accept scripts as interpreters, using #!/bin/sh and sourcing.
        if [ true != "$INIT_D_SCRIPT_SOURCED" ] ; then
            set "$0" "$@"; INIT_D_SCRIPT_SOURCED=true . /lib/init/init-d-script
        fi
        ### BEGIN INIT INFO
        # Provides:          codeworld
        # Required-Start:    $remote_fs $syslog
        # Required-Stop:     $remote_fs $syslog
        # Default-Start:     2 3 4 5
        # Default-Stop:      0 1 6
        # Short-Description: CodeWorld web site
        # Description:       Runs the CodeWorld web site on its default port.
        ### END INIT INFO
        # Author: Chris Smith <cdsmith@gmail.com>
        CODEWORLD=/home/cdsmith/codeworld
        DESC="CodeWorld web site"
        DAEMON=$CODEWORLD/build/bin/codeworld-server
        do_start() {
          cd $CODEWORLD
          sudo -u cdsmith nohup ./run.sh &
        }
        do_stop() {
          killall codeworld-server
        }
        do_reload() {
          do_stop
          do_start
        }

14. `sudo update-rc.d codeworld defaults`
15. `nohup ./run.sh &`
