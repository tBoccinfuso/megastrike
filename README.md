# Stage 5C.2B: Kevin final facing

Kevin picks a facing toward the nearest enemy after selecting a movement destination. The destination matters, so Kevin can turn toward enemies that end up behind his movement path. Facing is chosen before `confirm-move`, which consumes the existing movement turn.

The change does not rotate Kevin during Combat and does not modify manual unit controls or the battlefield renderer.

Installation:

    unzip -o ~/Downloads/megastrike-stage5c2b-kevin-facing.zip
    make test
    make run

Test Kevin's facing after each movement. This is basic nearest-enemy orientation, not yet a full risk-based rear-arc evaluator.
