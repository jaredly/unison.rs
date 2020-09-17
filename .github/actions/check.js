#!/usr/bin/env node

const fs = require('fs');
const TRIGGER = 'actions: release';

const setFailed = (message) => {
    process.exitCode = 1;
    console.log('::error::' + message);
};

async function run() {
    const payload = JSON.parse(
        fs.readFileSync(process.env.GITHUB_EVENT_PATH, { encoding: 'utf8' }),
    );
    const match = payload.head_commit.message.split('\n')[0].trim() === TRIGGER;

    if (!match) {
        setFailed('No trigger');
        return;
    }
}

run().catch((err) => {
    console.error(err);
    setFailed('Unexpected error');
});
