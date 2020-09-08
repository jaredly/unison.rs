import jsonEqual from '@birchill/json-equalish';

export const diff = (traceA, traceB) => {
    const loop = (idx) => {
        for (let i = 0; i < traceA[idx].events.length; i++) {
            console.log(idx, i);
            if (!jsonEqual(traceA[idx].events[i], traceB[idx].events[i])) {
                return [idx, i];
            }
            const k = Object.keys(traceA[idx].events[i])[0];
            if (k === 'NewFrame' || k === 'CloneFrame') {
                //} || k === 'JumpBack') {
                const inner = loop(traceA[idx].events[i][k]);
                if (inner != null) {
                    return inner;
                }
            }
            // if (traceA[idx].events[i].NewFrame) {
            //     const inner = loop(traceA[idx].events[i].NewFrame);
            //     if (inner != null) {
            //         return inner;
            //     }
            // }
            // if (traceA[idx].events[i].CloneFrame) {
            //     const inner = loop(traceA[idx].events[i].CloneFrame);
            //     if (inner != null) {
            //         return inner;
            //     }
            // }
        }
    };
    const res = loop(0);
    if (res != null) {
        console.log(`Found it!`, res);
        window.off = [traceA[res[0]][res[1]], traceB[res[0]][res[1]]];
    }
};

// export const diff = (a, b) => {
//     for (let i = 0; i < a.length; i++) {
//         if (!jsonEqual(a[i].events, b[i].events.slice(0, a[i].events.length))) {
//             console.log('evt', i);
//             for (let j = 0; j < a[i].events.length; j++) {
//                 if (!jsonEqual(a[i].events[j], b[i].events[j])) {
//                     console.log(`Found it! ${i} - ${j}`);
//                     console.log(a[i].events[j]);
//                     console.log(b[i].events[j]);
//                     window.off = [a[i].events[j], b[i].events[j]];
//                     return;
//                 }
//             }
//             console.log(a[i].events.length);
//             break;
//         }
//     }
// };
