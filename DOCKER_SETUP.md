
1. source docker-setup.sh
2. source environment-source.sh
3. source sourceme-manager.sh --skip-ssh-setup

Now, verify you can `ssh localhost`

make sure there's a /external-workspace/chipyard/sims/firesim/deploy/config_runtime.yaml

firesim infrasetup -r $CYDIR/sims/firesim-staging/sample_config_build_recipes.yaml
firesim runworkload -r $CYDIR/sims/firesim-staging/sample_config_build_recipes.yaml