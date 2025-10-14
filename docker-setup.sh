# Remove old keys
/usr/sbin/sshd -D &
ssh-keygen -f "/root/.ssh/known_hosts" -R "localhost"


# Replace PermitRootLogins
sed -i 's/PermitRootLogin no/PermitRootLogin yes/' /etc/ssh/sshd_config

# Add a new ed25519 key 
ssh-keygen -t ed25519 -f /root/.ssh/id_ed25519 -N ""
chmod 600 /root/.ssh/id_ed25519
chmod 700 /root/.ssh

cat /root/.ssh/id_ed25519.pub >> /root/.ssh/authorized_keys
chmod 600 /root/.ssh/authorized_keys