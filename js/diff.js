import jsonEqual from '@birchill/json-equalish';

export const diff = (a, b) => {
    for (let i = 0; i < a.length; i++) {
        if (!jsonEqual(a[i].events, b[i].events.slice(0, a[i].events.length))) {
            console.log('evt', i);
            for (let j = 0; j < a[i].events.length; j++) {
                if (!jsonEqual(a[i].events[j], b[i].events[j])) {
                    console.log(`Found it! ${i} - ${j}`);
                    console.log(a[i].events[j]);
                    console.log(b[i].events[j]);
                    window.off = [a[i].events[j], b[i].events[j]];
                    return;
                }
            }
            console.log(a[i].events.length);
            break;
        }
    }
};
