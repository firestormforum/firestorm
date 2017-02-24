#!/usr/bin/env node

'use strict';

const minimist = require('minimist');
const camelCase = require('lodash.camelcase');
const checkDependencies = require('../lib/check-dependencies');

const argv = minimist(process.argv.slice(2));

// camelCase the options
for (const key of Object.keys(argv)) {
    const value = argv[key];
    delete argv[key];
    argv[camelCase(key)] = value;
}

// Options of type array should always have array values
for (const option of ['scopeList', 'optionalScopeList']) {
    if (option in argv) {
        if (!Array.isArray(argv[option])) {
            argv[option] = [argv[option]];
        }
    }
}

// We'll handle verbosity by the CLI here.
const verbose = argv.verbose;
delete argv.verbose;

const Cli = {
    reporter(result) {
        if (verbose) {
            for (const msg of result.log) {
                console.log(msg);
            }
        }

        for (const msg of result.error) {
            console.error(msg);
        }

        if (result.status !== 0) {
            process.exitCode = result.status;
        }
    },
};

checkDependencies(argv, Cli.reporter);

module.exports = Cli;
