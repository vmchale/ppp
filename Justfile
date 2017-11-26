next VERSION:
    @git tag v{{ VERSION }}
    @bower install
    @pulp publish

test:
    @pulp --psc-package test

build:
    @pulp --psc-package build
