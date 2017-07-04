main             = simulationOf(step, picture)
step((x, y), dt) = (x - y * dt, y + x * dt)
picture(x, y)    = translated(rectangle(1, 1), x, y)

